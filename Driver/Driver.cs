//
// Salsa .NET Driver
//
// Licence:   BSD3 (see LICENSE)
//

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Net;
using System.Reflection;
using System.Reflection.Emit;
using System.Diagnostics;
using System.IO;
using System.Threading;

[assembly: AssemblyTitle("Salsa")]
[assembly: AssemblyDescription(".NET Bridge for Haskell")]
[assembly: AssemblyProduct("Salsa")]
[assembly: AssemblyCopyright("Copyright © 2007-2008 Andrew Appleyard")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyFileVersion("1.0.0.0")]

// TODO: Consider caching the results of the Get*Stub(string, ...) methods
//       (although it's not really necessary, Haskell will cache the results).

namespace Salsa
{
    public class Driver
    {
        internal static AssemblyBuilder _assemblyBuilder;
        internal static ModuleBuilder _dynamicModuleBuilder;
        internal static TypeBuilder _stubsTypeBuilder;

        static Driver()
        {
            //Console.WriteLine("Using Salsa.dll (version {0})",
            //    Assembly.GetExecutingAssembly().GetName().Version);

            Trace.Listeners.Add(new ConsoleTraceListener());

            _assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(
                new AssemblyName("DynamicAssembly"), AssemblyBuilderAccess.RunAndSave);
            _dynamicModuleBuilder = _assemblyBuilder.DefineDynamicModule("DynamicModule", "Dynamic.dll");
            _stubsTypeBuilder = _dynamicModuleBuilder.DefineType("Stubs");

            _inTable.Add(_nextId++, true);  // ObjectId 1
            _inTable.Add(_nextId++, false); // ObjectId 2
        }

        public static IntPtr Boot()
        {
            // TODO: Accept an option from Salsa for the threading model to use
            // Thread.CurrentThread.SetApartmentState(ApartmentState.STA);

            return GetPointerToMethod("GetPointerToMethod");
        }

        /// <summary>
        /// Returns a native function pointer (as an int) to a stub wrapping the given method.
        /// </summary>
        /// <remarks>
        /// This is the first .NET function called by Haskell, and is called by using the 
        /// ICLRRuntimeHost.ExecuteInDefaultAppDomain method.
        /// </remarks>
        public static IntPtr GetPointerToMethod(string methodName)
        {
            //Trace.WriteLine("GetPointerToMethod(" + methodName + ")");
            //Console.WriteLine("Called on thread: " + System.Threading.Thread.CurrentThread.ManagedThreadId);

            MethodInfo meth = typeof(Driver).GetMethod(methodName);
            if (meth == null)
                throw new ArgumentException("Method not found.", "methodName");

            // Return a delegate pointing to the indicated method (cast to an int 
            // so we can call this method using ExecuteInDefaultAppDomain)
            return GenerateMethodStub(meth);
        }

        #region Entry points

        /// <summary>
        /// This method is called by Haskell so that the .NET side of the bridge
        /// has access to the Haskell function 'freeHaskellFunPtr'.
        /// </summary>
        public static void SetFreeHaskellFunPtr(IntPtr freeHaskellFunPtr)
        {
            if (freeHaskellFunPtr == IntPtr.Zero)
            {
                // Clear any previously stored delegate wrapping 'freeHaskellFunPtr'
                // (this will prevent calls into Haskell from .NET finalizers)
                FreeHaskellFunPtr = null;
            }
            else
            {
                // Store the delegate wrapping the 'freeHaskellFunPtr' function passed in
                FreeHaskellFunPtr =
                    (FreeHaskellFunPtrDelegate)Marshal.GetDelegateForFunctionPointer(
                        freeHaskellFunPtr, typeof(FreeHaskellFunPtrDelegate));
            }
            Trace.WriteLine(string.Format("SetFreeHaskellFunPtr(0x{0:x})", freeHaskellFunPtr));
        }

        public static FreeHaskellFunPtrDelegate FreeHaskellFunPtr;

        /// <summary>
        /// Saves the assembly containing the dynamically generated wrapper methods and 
        /// delegate classes created during the bridge operation.
        /// </summary>
        public static void SaveDynamicAssembly()
        {
            _stubsTypeBuilder.CreateType();

            string fileName = Path.GetFileName(_dynamicModuleBuilder.FullyQualifiedName);
            Console.WriteLine("Saving dynamic assembly: " + fileName);
            _assemblyBuilder.Save(fileName);
        }

        /// <summary>
        /// Given the class, name, and signature of a method or constructor, returns a 
        /// native function pointer to a delegate that calls the method or constructor
        /// when invoked.
        /// </summary>
        /// <remarks>
        /// Use a 'methodName' of '.ctor' to obtain a constructor stub.
        /// </remarks>
        public static IntPtr GetMethodStub(string className, string methodName,
            string parameterTypeNames)
        {
            if (methodName == ".ctor")
            {
                ConstructorInfo con = Util.StringToType(className).GetConstructor(
                    Util.StringToTypes(parameterTypeNames));
                return GenerateConstructorStub(con);
            }
            else
            {
                MethodInfo meth = Util.StringToType(className).GetMethod(
                    methodName, Util.StringToTypes(parameterTypeNames));
                return GenerateMethodStub(meth);
            }
        }

        /// <summary>
        /// Given the name of a delegate type, returns a function pointer to a delegate that,
        /// when called, instantiates delegate instances of the given delegate type when given
        /// a pointer to a Haskell function.
        /// </summary>
        public static IntPtr GetDelegateConstructorStub(string delegateTypeName)
        {
            Type delegateType = Util.StringToType(delegateTypeName);
            return GenerateDelegateConstructorStub(delegateType);
        }

        /// <summary>
        /// Given the class and name of a field, returns a native function pointer to a 
        /// delegate that returns the value of the field when invoked.
        /// </summary>
        public static IntPtr GetFieldGetStub(string className, string fieldName)
        {
            FieldInfo field = Util.StringToType(className).GetField(fieldName);
            return GenerateFieldGetStub(field);
        }

        /// <summary>
        /// Given the class and name of a field, returns a native function pointer to a 
        /// delegate that sets the value of the field when invoked.
        /// </summary>
        public static IntPtr GetFieldSetStub(string className, string fieldName)
        {
            FieldInfo field = Util.StringToType(className).GetField(fieldName);
            return GenerateFieldSetStub(field);
        }

        /// <summary>
        /// Given a value type, returns a native function pointer to a method that 
        /// boxes values of the given value type.
        /// </summary>
        public static IntPtr GetBoxStub(string typeName)
        {
            Type typeToBox = Util.StringToType(typeName);
            return GenerateBoxStub(typeToBox);
        }

        #endregion

        #region Foreign object references (object in-table)

        /// <summary>
        /// Maps object identifiers to .NET references.  Allows foreign object identifiers 
        /// to be dereferenced to .NET object references.  Also ensures that .NET objects 
        /// referred to from Haskell are kept alive.
        /// </summary>
        static Dictionary<int, object> _inTable = new Dictionary<int, Object>();

        static int _nextId = 1;

        /// <summary>
        /// Registers the given object in the 'in table' and returns the object id
        /// that was assigned to it.
        /// </summary>
        public static int RegisterObject(object o)
        {
            if (o == null) 
                return 0;

            if (o is bool?) // HACK for Nullable<bool>
            {
                bool b = (bool)o;
                return b ? 1 : 2;
            }

            lock (_inTable)
            {
                _inTable.Add(_nextId, o);
                return _nextId++;
            }
        }

        public static object GetObject(int oId)
        {
            if (oId == 0) // 0 represents a null reference
                return null;

            if (oId == 1) // 1 represents boxed true
                return true; // TODO: Return pre-boxed instance?
            if (oId == 2) // 0 represents boxed false
                return false;

            lock (_inTable)
            {
                object o;
                if (_inTable.TryGetValue(oId, out o))
                    return o;
                else
                {
                    throw new ArgumentException("No object exists with id: " + oId);
                    // FIXME: This exception will occur in the current implementation of the 
                    //        bridge if a Haskell garbage collection (and finalization) runs 
                    //        while a Haskell-implemented delegate is returning an object.
                    //        Given that this condition tends not to occur with the current
                    //        implementation of the GHC RTS, and that most delegates in .NET
                    //        don't return a value at all, I'm leaving the fix for later.
                }
            }
        }

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate void ReleaseObjectDelegate(int oId);
        private static ReleaseObjectDelegate _ReleaseObjectDelegate = ReleaseObject;

        public static void ReleaseObject(int oId)
        {
            lock (_inTable)
            {
                _inTable.Remove(oId);
            }
        }

        #endregion

        #region Managed delegate references (function pointer in-table)

        /// <summary>
        /// Maintains references to any .NET delegates that have a native function pointer
        /// referring to them (as obtained using Marshal.GetFunctionPointerForDelegate), so 
        /// that they are not collected prematurely.
        /// </summary>
        private static Dictionary<IntPtr, Delegate> _dotNetFunPtrDelegates = new Dictionary<IntPtr, Delegate>();

        /// <summary>
        /// Returns a native function pointer for calling the given .NET delegate.  A 
        /// reference to the delegate is stored in _dotNetFunPtrDelegates to prevent
        /// the delegate from being garbage collected while the function pointer is 
        /// still being used.
        /// </summary>
        private static IntPtr GetDotNetFunPtrForDelegate(Delegate d) // TODO: Perhaps rename 'RegisterDelegate'
        {
            IntPtr funPtr = Marshal.GetFunctionPointerForDelegate(d);
            lock (_dotNetFunPtrDelegates)
                _dotNetFunPtrDelegates.Add(funPtr, d);
            return funPtr;
        }

        /// <summary>
        /// This method is called to indicate that the given native function pointer 
        /// will no longer be called from Haskell.  It allows the associated .NET
        /// delegate to be garbage collected provided there are no other references
        /// to it.
        /// </summary>
        public static void FreeDotNetFunPtr(IntPtr funPtr) // TODO: Perhaps rename 'ReleaseDelegate'
        {
            lock (_dotNetFunPtrDelegates)
                _dotNetFunPtrDelegates.Remove(funPtr);
        }

        #endregion

        #region Stub method generation

        /// <summary>
        /// Generates a dynamic method of the given name and signature, containing the instructions 
        /// produced by 'ilWriter'.  A delegate to the method, is returned to the caller as a native
        /// function pointer.
        /// </summary>
        /// <remarks>
        /// This method also writes the method to the 'Stubs' class in 'Dynamic.dll' for debugging 
        /// purposes.
        /// </remarks>
        private static IntPtr GenerateDynamicMethod(string methodName,
            DelegateSignature methodSignature, ILWriterDelegate ilWriter)
        {
            if (true)
            {
                // Optional: generate an implementation of the stub method inside the 'Stubs'
                //           class (which is saved to 'Dynamic.dll') for debugging purposes.
                MethodBuilder savedMethod = _stubsTypeBuilder.DefineMethod(methodName, MethodAttributes.Public,
                    methodSignature.ReturnType, methodSignature.ParameterTypes);
                ilWriter(savedMethod.GetILGenerator());
            }

            // Generate a dynamic method of the given name and signature, and the 
            // IL instructions given by 'ilWriter', then return a delegate to the
            // method as a function pointer.

            // [stubReturnType] [classType]_[methodname]([classType] this, [args...]):
            DynamicMethod method = new DynamicMethod(methodName, methodSignature.ReturnType,
                methodSignature.ParameterTypes, typeof(Driver));
            ilWriter(method.GetILGenerator());

            return GetDotNetFunPtrForDelegate(method.CreateDelegate(methodSignature.ToDelegateType()));
        }

        /// <summary>
        /// Returns a native function pointer to a wrapper stub that accepts foreign 
        /// parameters, marshals them, calls the indicated constructor, and returns the
        /// resulting object instance (marshaled as necessary).
        /// </summary>
        /// <remarks>
        /// The returned function pointer should be freed with 'FreeDotNetFunPtr'.
        /// When used to construct a value type, 'con' may be null, in which case 
        /// no constructor is called (the value is just initialised to zero).
        /// </remarks>
        public static IntPtr GenerateConstructorStub(ConstructorInfo con)
        {
            Type type = con.DeclaringType;

            if (!type.IsValueType && con == null)
                throw new ArgumentNullException("'con' cannot be null if creating a reference type.");

            // Generate a dynamic method that does the following:
            //
            //     Int32 [classType]New([args...])
            //     {
            //         [classType] o = new [classType]([args... using GetObject ... ]);
            //         (box o if it is a value type)
            //         return Driver.RegisterObject(o);
            //     }

            // Signature of the stub method returned (as a delegate) by this function
            DelegateSignature methodSignature = new DelegateSignature(
                typeof(Int32), 
                con == null ? Type.EmptyTypes : 
                              ConvertToStubTypes(Util.MapParametersToTypes(con.GetParameters())));

            return GenerateDynamicMethod(type.Name + "New", methodSignature,
                delegate(ILGenerator ilg)
                {
                    if (con == null) // Initialise object to zero?
                    {
                        // Initialise the object in a local variable
                        ilg.DeclareLocal(type);
                        ilg.Emit(OpCodes.Ldloca_S, (byte)0);
                        ilg.Emit(OpCodes.Initobj, type);

                        // Load the object onto the stack
                        ilg.Emit(OpCodes.Ldloc_0);
                    }
                    else // Call object constructor:
                    {
                        // Load (and unmarshal), the arguments to the constructor stub
                        EmitParameterLoading(ilg, 0, con.GetParameters());

                        // Call the constructor, i.e. 'o = new [classType]([unmarshaled args])'
                        ilg.Emit(OpCodes.Newobj, con);
                    }

                    // Return the new object (marshaled as an index)
                    EmitToStub(ilg, type);
                    ilg.Emit(OpCodes.Ret);
                });
        }

        /// <summary>
        /// Returns a native function pointer to a stub method that calls the given
        /// method, marshaling arguments as necessary.
        /// </summary>
        /// <remarks>
        /// The returned function pointer should be freed with 'FreeDotNetFunPtr'.
        /// </remarks>
        public static IntPtr GenerateMethodStub(MethodInfo meth)
        {
            Type classType = meth.DeclaringType;

            // Generate a dynamic method that does the following:
            //
            //     [stubReturnType] [classType]_[methodName]([classType] this, [args...])
            //       OR (if a static method)
            //     [stubReturnType] [classType]_[methodName]([args...])
            //     {
            //         [resultType] r = this.[methodName]([marshaledThis], [marshaled args]);
            //           OR (if a static method)
            //         [resultType] r = [className].[methodName]([marshaled args]);
            //
            //         return r; OR return Driver.RegisterObject(r);
            //     }

            // Signature of the stub method returned (as a delegate) by this function
            DelegateSignature methodSignature = new DelegateSignature(
                ConvertToStubType(meth.ReturnParameter.ParameterType),
                meth.IsStatic ? ConvertToStubTypes(Util.MapParametersToTypes(meth.GetParameters())) :
                    Util.ConcatArray<Type>(
                        ConvertToStubType(meth.DeclaringType),
                        ConvertToStubTypes(Util.MapParametersToTypes(meth.GetParameters()))));

            return GenerateDynamicMethod(classType.Name + "_" + meth.Name, methodSignature,
                delegate(ILGenerator ilg)
                {
                    // Load (and unmarshal), the arguments to the method stub, then call the real method
                    if (meth.IsStatic)
                    {
                        EmitParameterLoading(ilg, 0, meth.GetParameters());
                        ilg.Emit(OpCodes.Call, meth);
                    }
                    else
                    {
                        EmitParameterLoading(ilg, 0, meth.DeclaringType);
                        EmitParameterLoading(ilg, 1, meth.GetParameters());
                        ilg.Emit(OpCodes.Callvirt, meth);
                    }

                    // Unmarshal and return the result
                    EmitMarshaledReturn(ilg, meth.ReturnParameter);
                });
        }

        /// <summary>
        /// Instantiates a delegate that, given an IntPtr to a Haskell function, returns a
        /// delegate instance (of the given delegate type) that wraps this native function
        /// (and deals with parameter value marshaling and finalization).
        /// </summary>
        /// <param name="delegateType">Type of delegate returned by the returned delegate.</param>
        /// <returns>
        /// Wrapper delegate that accepts an IntPtr and returns a delegate of the
        /// indicated type.
        /// </returns>
        public static IntPtr GenerateDelegateConstructorStub(Type delegateType)
        {
            // Obtain (creating, if necessary) the type of a wrapper class for the delegate
            Type wrapperType = GetDelegateWrapperType(delegateType);

            DelegateSignature delegateSignature = DelegateSignature.FromDelegateType(delegateType);

            // Generate a dynamic method that does the following:
            //
            //     Int32 [delegateType]New(IntPtr funPtr)
            //     {
            //         [wrapperType] wrapper = new [wrapperType](funPtr);
            //         Delegate d = new [delegateType](wrapper.Invoke);
            //         return Driver.RegisterObject(d);
            //     }

            // Signature of the method returned (as a delegate) by this function (the
            // method accepts an IntPtr to a Haskell function and returns an instantiated
            // .NET delegate for it)
            DelegateSignature methodSignature = new DelegateSignature(
                typeof(Int32), new Type[] { typeof(IntPtr) });

            return GenerateDynamicMethod(delegateType.Name + "New", methodSignature,
                delegate(ILGenerator ilg)
                {
                    // wrapper = new [wrapperType](funPtr):
                    ilg.Emit(OpCodes.Ldarg_0);
                    ilg.Emit(OpCodes.Newobj, wrapperType.GetConstructor(new Type[] { typeof(IntPtr) }));

                    // Obtain wrapper.Invoke
                    ilg.Emit(OpCodes.Ldftn, wrapperType.GetMethod("Invoke"));

                    // Delegate d = new [delegateType](wrapper.Invoke):
                    ilg.Emit(OpCodes.Newobj, delegateType.GetConstructor(new Type[] { typeof(object), typeof(IntPtr) }));

                    // return Driver.RegisterObject(d):
                    ilg.Emit(OpCodes.Call, MemberInfos.Driver_RegisterObject);
                    ilg.Emit(OpCodes.Ret);
                });
        }

        /// <summary>
        /// Returns a native function pointer to a stub method that retrieves the value
        /// of the given static or instance field.
        /// </summary>
        /// <remarks>
        /// The returned function pointer should be freed with 'FreeDotNetFunPtr'.
        /// </remarks>
        public static IntPtr GenerateFieldGetStub(FieldInfo field)
        {
            // Signature of the stub method returned (as a delegate) by this function
            DelegateSignature methodSignature = new DelegateSignature(
                ConvertToStubType(field.FieldType),
                field.IsStatic ? Type.EmptyTypes : new Type[] { ConvertToStubType(field.DeclaringType) });

            return GenerateDynamicMethod(field.DeclaringType.Name + "_field_get_" + field.Name, methodSignature,
                delegate(ILGenerator ilg)
                {
                    if (field.IsStatic)
                    {
                        if (field.IsLiteral)
                        {
                            // FIXME: Move literal calculation to the generator, and add
                            //        support for literal values in the bridge (i.e.
                            //        values other than 'Obj ObjectId's
                            object literalValue = field.GetRawConstantValue();
                            if (literalValue is Int32)
                                ilg.Emit(OpCodes.Ldc_I4, (Int32)literalValue);
                        }
                        else
                            ilg.Emit(OpCodes.Ldsfld, field);
                    }
                    else
                    {
                        // Load (and unmarshal) the argument to the method stub, then load the field value
                        EmitParameterLoading(ilg, 0, field.DeclaringType);
                        ilg.Emit(OpCodes.Ldfld, field);
                    }

                    // Unmarshal and return the result
                    EmitToStub(ilg, field.FieldType);
                    ilg.Emit(OpCodes.Ret);
                });
        }

        /// <summary>
        /// Returns a native function pointer to a stub method that sets the value
        /// of the given static or instance field.
        /// </summary>
        /// <remarks>
        /// The returned function pointer should be freed with 'FreeDotNetFunPtr'.
        /// </remarks>
        public static IntPtr GenerateFieldSetStub(FieldInfo field)
        {
            // Signature of the stub method returned (as a delegate) by this function
            DelegateSignature methodSignature = new DelegateSignature(
                typeof(void),
                field.IsStatic ? 
                    new Type[] { ConvertToStubType(field.FieldType) } : 
                    new Type[] { ConvertToStubType(field.DeclaringType), ConvertToStubType(field.FieldType) });

            return GenerateDynamicMethod(field.DeclaringType.Name + "_field_set_" + field.Name, methodSignature,
                delegate(ILGenerator ilg)
                {
                    if (field.IsStatic)
                    {
                        // Load (and unmarshal) the field value from the stub call
                        EmitParameterLoading(ilg, 0, field.FieldType);
                        ilg.Emit(OpCodes.Stsfld, field);
                    }
                    else
                    {
                        // Load (and unmarshal) the instance argument and field value from the stub call
                        EmitParameterLoading(ilg, 0, field.DeclaringType);
                        EmitParameterLoading(ilg, 1, field.FieldType);
                        ilg.Emit(OpCodes.Stfld, field);
                    }

                    ilg.Emit(OpCodes.Ret);
                });
        }

        /// <summary>
        /// Returns a native function pointer to a stub method that returns a 
        /// reference to the given value (boxing value types as necessary).
        /// </summary>
        /// <remarks>
        /// The returned function pointer should be freed with 'FreeDotNetFunPtr'.
        /// </remarks>
        public static IntPtr GenerateBoxStub(Type typeToBox)
        {
            // Signature of the stub method returned (as a delegate) by this function
            DelegateSignature methodSignature = new DelegateSignature(
                typeof(Int32), new Type[] { ConvertToStubType(typeToBox) });

            return GenerateDynamicMethod("box_" + typeToBox.Name, methodSignature,
                delegate(ILGenerator ilg)
                {
                    // Load the (unboxed) value
                    EmitParameterLoading(ilg, 0, typeToBox);

                    if (typeToBox.IsValueType)
                    {
                        // Box the type value to as an object
                        ilg.Emit(OpCodes.Box, typeToBox);
                    }

                    // Marshal the object instance out as an object index
                    EmitToStub(ilg, typeof(object));
                    ilg.Emit(OpCodes.Ret);
                });
        }

        #endregion

        #region Parameter and result marshaling

        private static bool IsMarshaledByIndex(Type t)
        {
            return !t.IsPrimitive && t != typeof(void) && t != typeof(string);
        }

        /// <summary>
        /// Returns the type of 'MarshalAs' attribute that should be attached to 
        /// parameters/results of the given type, if any.
        /// </summary>
        private static UnmanagedType? MarshalTypeAs(Type t)
        {
            if (t == typeof(string))
                return UnmanagedType.LPWStr;
            else
                return null;
        }

        /// <summary>
        /// Given an array of parameters (say of a constructor, or method), returns the 
        /// array of types that a corresponding wrapper stub would accept.
        /// </summary>
        private static Type[] ConvertToStubTypes(Type[] types)
        {
            return Util.MapArray<Type, Type>(ConvertToStubType, types);
        }

        private static Type ConvertToStubType(Type type)
        {
            if (IsMarshaledByIndex(type))
                return typeof(Int32);
            else
                return type;
        }

        /// <summary>
        /// Emits code (via the given IL generator) to load a single parameter of the given 
        /// type from the given argument index and on to the stack.  It ensures that object 
        /// types (which are marshaled by index), are resolved to the appropriate .NET object 
        /// instance.
        /// </summary>
        private static void EmitParameterLoading(ILGenerator ilg, int argumentIndex, Type parameterType)
        {
            Util.EmitLdarg(ilg, argumentIndex);
            EmitFromStub(ilg, parameterType);
        }

        // Converts a value on the stack from a stub stub to the associated 'real' .NET type
        /// <summary>
        /// Emits code to convert a value on the stack from a stub type to the associated
        /// 'real' .NET type.
        /// </summary>
        /// <remarks>
        /// For example, when called for the 'Button' type, code is emitted to convert an 
        /// Int32 object identifier into a Button; but when called with the 'String' type 
        /// no code is emitted at all since the stub type matches the desired .NET type 
        /// exactly.
        /// </remarks>
        private static void EmitFromStub(ILGenerator ilg, Type valueType)
        {
            if (IsMarshaledByIndex(valueType))
            {
                // [valueType]x = ([valueType])GetObject(value):

                // Call GetObject on the object index to obtain the object instance
                ilg.Emit(OpCodes.Call, MemberInfos.Driver_GetObject);

                // Cast from Object to the appropriate type for the value (unboxing if necessary)
                ilg.Emit(OpCodes.Unbox_Any, valueType);

                // Note, the above instruction is equivalent to:
                //
                //   if (valueType.IsValueType)
                //   {
                //       ilg.Emit(OpCodes.Unbox, valueType);
                //       ilg.Emit(OpCodes.Ldobj);
                //   }
                //   else
                //       ilg.Emit(OpCodes.Castclass, valueType);
                //
            }
            else
            {
                // Leave value as is
            }
        }

        /// <summary>
        /// Emits code (via the given IL generator) to load parameters of the given types 
        /// from the arguments of the method (starting with 'startingArgument') and on to 
        /// the stack.  It ensures that object types (which are marshaled by index), are 
        /// resolved to the appropriate .NET object instance.
        /// </summary>
        private static void EmitParameterLoading(ILGenerator ilg, int startingArgument,
            IEnumerable<ParameterInfo> parameterInfos)
        {
            int argumentIndex = startingArgument;
            foreach (ParameterInfo parameterInfo in parameterInfos)
            {
                Type parameterType = parameterInfo.ParameterType;
                Util.EmitLdarg(ilg, argumentIndex++);
                EmitFromStub(ilg, parameterType);
            }
        }

        private static void EmitToStub(ILGenerator ilg, Type type)
        {
            if (IsMarshaledByIndex(type))
            {
                if (type.IsValueType)
                {
                    // Box the value type for passing it to RegisterObject
                    ilg.Emit(OpCodes.Box, type);
                }

                // Marshal the object instance out as an object index, by
                // calling RegisterObject
                ilg.Emit(OpCodes.Call, MemberInfos.Driver_RegisterObject);
            }
            else
            {
                // Leave value as is
            }
        }

        /// <summary>
        /// Emits code (via the given IL generator) to return the value on the top of the
        /// stack, knowing that its type is given by 'returnParameterInfo'.  It ensures 
        /// that object types (which are marshaled by index), are returned as an index.
        /// </summary>
        private static void EmitMarshaledReturn(ILGenerator ilg, ParameterInfo returnParameterInfo)
        {
            Type parameterType = returnParameterInfo.ParameterType;
            EmitToStub(ilg, parameterType);
            ilg.Emit(OpCodes.Ret);
        }

        #endregion

        #region Delegate wrappers

        /// <summary>
        /// Maintains a cache of the delegate wrapper types that have been generated.
        /// </summary>
        private static Dictionary<string, Type> _delegateWrapperTypes =
            new Dictionary<string, Type>();

        private static Type GetDelegateWrapperType(Type delegateType)
        {
            string wrapperTypeName = delegateType.Name + "Wrapper";
            lock (_delegateWrapperTypes)
            {
                Type type;
                if (!_delegateWrapperTypes.TryGetValue(wrapperTypeName, out type))
                {
                    // Could not find wrapper class of appropriate type in the cache: create one
                    type = CreateDelegateWrapperType(wrapperTypeName, delegateType);
                    _delegateWrapperTypes.Add(wrapperTypeName, type);
                }
                return type;
            }
        }

        /// <summary>
        /// Creates (and returns the type of) a class that wraps a pointer to a
        /// Haskell function as a .NET delegate.  The class ensures that the
        /// wrapper function pointer is freed when the delegate is no longer being 
        /// used by .NET.  It also performs any translation from .NET values to
        /// interop values (object references are converted to object identifiers).
        /// </summary>
        /// <param name="delegateType">Type of delegate produced by the wrapper</param>
        private static Type CreateDelegateWrapperType(string name, Type delegateType)
        {
            // Obtain the signature of the delegate being produced
            DelegateSignature delegateSignature = DelegateSignature.FromDelegateType(delegateType);

            // Obtain the delegate type of the associated thunk for calling into Haskell
            Type thunkDelegateType = new DelegateSignature(
                ConvertToStubType(delegateSignature.ReturnType),
                ConvertToStubTypes(delegateSignature.ParameterTypes)).ToDelegateType();

            TypeBuilder typeBuilder = _dynamicModuleBuilder.DefineType(name,
                TypeAttributes.Public | TypeAttributes.Sealed);

            // Define the _thunkDelegate field
            FieldBuilder thunkDelegateField = typeBuilder.DefineField("_thunkDelegate",
                thunkDelegateType, FieldAttributes.Private);

            {
                // Define the constructor for the wrapper type:
                //
                //     public DelegateWrapper(IntPtr funPtrToWrap)
                //     {
                //         _thunkDelegate = (ThunkDelegate)
                //             Marshal.GetDelegateForFunctionPointer(
                //                 funPtrToWrap, typeof(ThunkDelegate));
                //     }

                ConstructorBuilder constructorBuilder = typeBuilder.DefineConstructor(
                    MethodAttributes.Public | MethodAttributes.RTSpecialName,
                    CallingConventions.Standard, new Type[] { typeof(IntPtr) });
                ILGenerator ilg = constructorBuilder.GetILGenerator();

                // Call Object's constructor
                ilg.Emit(OpCodes.Ldarg_0);
                ilg.Emit(OpCodes.Call, MemberInfos.Object_ctor);

                ilg.Emit(OpCodes.Ldarg_0); // Load this (for the 'stfld' below)
                ilg.Emit(OpCodes.Ldarg_1); // Load funPtrToWrap

                ilg.Emit(OpCodes.Ldtoken, thunkDelegateType); // Load typeof(ThunkDelegate)
                ilg.Emit(OpCodes.Call, MemberInfos.Type_GetTypeFromHandle);

                // Call (ThunkDelegate)Marshal.GetDelegateForFunctionPointer(
                //          funPtrToWrap, typeof(ThunkDelegate))
                ilg.Emit(OpCodes.Call, MemberInfos.Marshal_GetDelegateForFunctionPointer);
                ilg.Emit(OpCodes.Castclass, thunkDelegateType);

                // Store in _thunkDelegate
                ilg.Emit(OpCodes.Stfld, thunkDelegateField);

                ilg.Emit(OpCodes.Ret);
            }

            {
                // Define a Finalize method for the wrapper class:
                //
                //     ~Delegate()
                //     {
                //         if (Driver.FreeHaskellFunPtr != null)
                //             Driver.FreeHaskellFunPtr(Marshal.GetFunctionPointerForDelegate(
                //                 _thunkDelegate));
                //     }

                MethodBuilder finalizeMethod = typeBuilder.DefineMethod("Finalize",
                    MethodAttributes.Family | MethodAttributes.HideBySig | MethodAttributes.Virtual);
                finalizeMethod.SetImplementationFlags(MethodImplAttributes.IL | MethodImplAttributes.Managed);
                ILGenerator ilg = finalizeMethod.GetILGenerator();
                Label endLabel = ilg.DefineLabel();

                // Obtain the freeHaskellFunPtr delegate
                ilg.Emit(OpCodes.Ldsfld, MemberInfos.Driver_FreeHaskellFunPtr);

                // Return immediately if the delegate is null
                ilg.Emit(OpCodes.Ldc_I4_0);
                ilg.Emit(OpCodes.Beq, endLabel);

                // Obtain the freeHaskellFunPtr delegate (again)
                ilg.Emit(OpCodes.Ldsfld, MemberInfos.Driver_FreeHaskellFunPtr);

                // Load _thunkDelegate
                ilg.Emit(OpCodes.Ldarg_0);
                ilg.Emit(OpCodes.Ldfld, thunkDelegateField);

                // Call Marshal.GetFunctionPointerForDelegate to obtain the original function pointerusing 
                ilg.Emit(OpCodes.Call, MemberInfos.Marshal_GetFunctionPointerForDelegate);

                // Invoke freeHaskellFunPtr on this function pointer
                ilg.Emit(OpCodes.Call, MemberInfos.FreeHaskellFunPtrDelegate_Invoke);

                ilg.MarkLabel(endLabel);
                ilg.Emit(OpCodes.Ret);
            }

            {
                // Define an Invoke method that calls _thunkDelegate after marshaling
                // the arguments as necessary:
                //
                //    private void Invoke(...)
                //    {
                //        _thunkDelegate(... using RegisterObject as appropriate ...);
                //    }

                MethodBuilder invokeMethod = typeBuilder.DefineMethod("Invoke",
                    MethodAttributes.Public, delegateSignature.ReturnType, delegateSignature.ParameterTypes);
                ILGenerator ilg = invokeMethod.GetILGenerator();

                // Load _thunkDelegate (for calling it later)
                ilg.Emit(OpCodes.Ldarg_0);
                ilg.Emit(OpCodes.Ldfld, thunkDelegateField);

                // Load the parameters (and marshal according to type)
                for (int i = 0; i < delegateSignature.ParameterTypes.Length; i++)
                {
                    Util.EmitLdarg(ilg, i + 1);
                    EmitToStub(ilg, delegateSignature.ParameterTypes[i]);
                }

                ilg.Emit(OpCodes.Callvirt, thunkDelegateType.GetMethod("Invoke"));
                EmitFromStub(ilg, delegateSignature.ReturnType);
                ilg.Emit(OpCodes.Ret);
            }

            return typeBuilder.CreateType();
        }

        #endregion

        #region DelegateSignature implementation

        public struct DelegateSignature
        {
            private Type _returnType;
            private Type[] _parameterTypes;

            public Type ReturnType
            {
                get { return _returnType; }
            }

            public Type[] ParameterTypes
            {
                get { return _parameterTypes; }
            }

            public DelegateSignature(Type returnType, Type[] parameterTypes)
            {
                _returnType = returnType;
                _parameterTypes = parameterTypes;
            }

            public override string ToString()
            {
                StringBuilder sb = new StringBuilder();
                foreach (Type parameter in _parameterTypes)
                {
                    sb.Append(parameter.Name);
                    sb.Append("To");
                }
                sb.Append(_returnType.Name);
                return sb.ToString();
            }

            public static DelegateSignature FromDelegateType(Type delegateType)
            {
                MethodInfo invokeMethod = delegateType.GetMethod("Invoke");
                return new DelegateSignature(
                    invokeMethod.ReturnType,
                    Util.MapParametersToTypes(invokeMethod.GetParameters()));
            }

            #region Delegate creation

            /// <summary>
            /// Maintains a cache of the delegate types that have been generated.
            /// </summary>
            private static Dictionary<string, Type> _delegateTypes =
                new Dictionary<string, Type>();

            /// <summary>
            /// Returns (after creating, if necessary) a delegate type of the given type signature.
            /// </summary>
            public Type ToDelegateType()
            {
                string delegateName = ToString() + "Delegate";
                lock (_delegateTypes)
                {
                    Type type;
                    if (!_delegateTypes.TryGetValue(delegateName, out type))
                    {
                        // Could not find delegate of appropriate type in the cache: create one
                        type = CreateDelegateType(delegateName);
                        _delegateTypes.Add(delegateName, type);
                    }
                    return type;
                }
            }

            /// <summary>
            /// Dynamically creates (and returns the type of) a delegate class with the given 
            /// name and type signature.
            /// </summary>
            private Type CreateDelegateType(string name)
            {
                TypeBuilder typeBuilder = Driver._dynamicModuleBuilder.DefineType(name,
                    TypeAttributes.Class | TypeAttributes.Public |
                    TypeAttributes.Sealed | TypeAttributes.AnsiClass |
                    TypeAttributes.AutoClass, typeof(System.MulticastDelegate));

                // Add a '[UnmanagedFunctionPointer(CallingConvention.Cdecl)]' attribute to the delegate
                typeBuilder.SetCustomAttribute(new CustomAttributeBuilder(
                    typeof(UnmanagedFunctionPointerAttribute).GetConstructor(new Type[] { typeof(CallingConvention) }),
                    new object[] { CallingConvention.Cdecl }));

                ConstructorBuilder constructorBuilder = typeBuilder.DefineConstructor(
                    MethodAttributes.RTSpecialName | MethodAttributes.HideBySig |
                    MethodAttributes.Public, CallingConventions.Standard,
                    new Type[] { typeof(object), typeof(System.IntPtr) });
                constructorBuilder.SetImplementationFlags(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);

                MethodBuilder methodBuilder = typeBuilder.DefineMethod("Invoke", MethodAttributes.Public |
                    MethodAttributes.HideBySig | MethodAttributes.NewSlot |
                    MethodAttributes.Virtual,
                    ReturnType, ParameterTypes);
                methodBuilder.SetImplementationFlags(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);

                // For the return type and any parameter types
                for (int i = 0; i < ParameterTypes.Length + 1; i++)
                {
                    UnmanagedType? marshalAs;
                    if (i == 0)
                        marshalAs = MarshalTypeAs(ReturnType);
                    else
                        marshalAs = MarshalTypeAs(ParameterTypes[i - 1]);

                    if (marshalAs != null)
                    {
                        // Add a MarshalAs attribute to the return/parameter
                        ParameterBuilder pb = methodBuilder.DefineParameter(i, ParameterAttributes.None, null);
                        pb.SetCustomAttribute(new CustomAttributeBuilder(
                                typeof(MarshalAsAttribute).GetConstructor(new Type[] { typeof(UnmanagedType) }),
                                new object[] { marshalAs.Value }));
                    }
                }
                return typeBuilder.CreateType();
            }

            #endregion
        }

        #endregion
    }

    /// <summary>
    /// A delegate for the signature of the Haskell function 'freeHaskellFunPtr'.
    /// </summary>
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
    public delegate void FreeHaskellFunPtrDelegate(IntPtr funPtr);

    /// <summary>
    /// A delegate for code the emits IL instructions on demand.
    /// </summary>
    public delegate void ILWriterDelegate(ILGenerator ilg);

    /// <summary>
    /// Stores references to commonly used reflection object instances.
    /// </summary>
    internal static class MemberInfos
    {
        public static readonly MethodInfo Type_GetTypeFromHandle =
            typeof(Type).GetMethod("GetTypeFromHandle");

        public static readonly ConstructorInfo Object_ctor =
            typeof(object).GetConstructor(Type.EmptyTypes);

        public static readonly MethodInfo Marshal_GetDelegateForFunctionPointer =
            typeof(Marshal).GetMethod("GetDelegateForFunctionPointer");

        public static readonly MethodInfo Marshal_GetFunctionPointerForDelegate =
            typeof(Marshal).GetMethod("GetFunctionPointerForDelegate");

        public static readonly MethodInfo Marshal_StringToHGlobalUni =
            typeof(Marshal).GetMethod("StringToHGlobalUni");

        public static readonly MethodInfo Marshal_StringToHGlobalAnsi =
            typeof(Marshal).GetMethod("StringToHGlobalAnsi");

        public static readonly FieldInfo Driver_FreeHaskellFunPtr =
            typeof(Driver).GetField("FreeHaskellFunPtr", BindingFlags.Static | BindingFlags.Public);

        public static readonly MethodInfo Driver_RegisterObject =
            typeof(Driver).GetMethod("RegisterObject", BindingFlags.Static | BindingFlags.Public);

        public static readonly MethodInfo Driver_GetObject =
            typeof(Driver).GetMethod("GetObject", BindingFlags.Static | BindingFlags.Public);

        public static readonly MethodInfo FreeHaskellFunPtrDelegate_Invoke =
            typeof(FreeHaskellFunPtrDelegate).GetMethod("Invoke");
    }

    internal static class Util
    {
        public static Type StringToType(string s)
        {
            return Type.GetType(s, true);
        }

        public static Type[] StringToTypes(string s)
        {
            return Util.MapArray<string, Type>(delegate(string t)
                { return StringToType(t); },
                s.Split(new char[] { ';' }, StringSplitOptions.RemoveEmptyEntries));
        }

        public static T[] ConcatArray<T>(T[] a, T[] b)
        {
            T[] r = new T[a.Length + b.Length];
            for (int i = 0; i < a.Length; i++)
                r[i] = a[i];
            for (int i = 0; i < b.Length; i++)
                r[a.Length + i] = b[i];
            return r;
        }

        public static T[] ConcatArray<T>(T x, T[] b)
        {
            T[] r = new T[1 + b.Length];
            r[0] = x;
            for (int i = 0; i < b.Length; i++)
                r[1 + i] = b[i];
            return r;
        }

        public static B[] MapArray<A, B>(Func<A, B> f, A[] xs)
        {
            B[] r = new B[xs.Length];
            for (int i = 0; i < xs.Length; i++)
                r[i] = f(xs[i]);
            return r;
        }

        public delegate B Func<A, B>(A x1);

        public static Type[] MapParametersToTypes(ParameterInfo[] parameters)
        {
            return MapArray<ParameterInfo, Type>(
                delegate(ParameterInfo p) { return p.ParameterType; },
                parameters);
        }

        public static void EmitLdarg(ILGenerator ilg, int argumentIndex)
        {
            if (argumentIndex == 0) ilg.Emit(OpCodes.Ldarg_0);
            else if (argumentIndex == 1) ilg.Emit(OpCodes.Ldarg_1);
            else if (argumentIndex == 2) ilg.Emit(OpCodes.Ldarg_2);
            else if (argumentIndex == 3) ilg.Emit(OpCodes.Ldarg_3);
            else if (argumentIndex <= 255) ilg.Emit(OpCodes.Ldarg_S, (byte)argumentIndex);
            else ilg.Emit(OpCodes.Ldarg, (int)argumentIndex);
        }
    }
}
