//
// Salsa Binding Generator
//
// Licence:   BSD3 (see LICENSE)
//

using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Reflection;
using System.Linq;

namespace Generator
{
    class Generator
    {
        static void Main(string[] args)
        {
            if (args.Length == 0)
            {
                Console.WriteLine("Usage: {0} [input-files]",
                    Path.GetFileName(Environment.GetCommandLineArgs()[0]));
            }
            else
                new Generator().Run(args);
        }

        private TextWriter w;

        private Set<string> _labels = new Set<string>();
        private Set<string> _invokers = new Set<string>();

        /// <summary>
        /// Instead of using long strings for identifiers inside the binding, we generate 
        /// shorter, unique ids using this dictionary.
        /// </summary>
        private Dictionary<string, long> _uniqueIds = new Dictionary<string, long>();

        /// <summary>
        /// Set of types that are required by the types generated so far.
        /// </summary>
        private Set<Type> _requiredTypes = new Set<Type>();

        /// <summary>
        /// Set of types that have been generated so far.
        /// </summary>
        private Set<Type> _generatedTypes = new Set<Type>();

        /// <summary>
        /// Set of types that were originally requested to be generated.
        /// </summary>
        private Set<Type> _requestedTypes = new Set<Type>();

        private Dictionary<Type, List<string>> _requestedMembers = new Dictionary<Type, List<string>>();

        private long GetUnique(params string[] keys)
        {
            string key = Util.Join("\0", keys); // Combine the keys
            long id;
            if (_uniqueIds.TryGetValue(key, out id))
                return id;
            else
            {
                id = _uniqueIds.Count + 1;
                _uniqueIds.Add(key, id);
                return id;
            }
        }

        private void RequestAll(Type t)
        {
            _requestedTypes.Add(t);

            List<string> ms;// = new List<string>();
            if (!_requestedMembers.TryGetValue(t, out ms))
                ms = new List<string>();

            foreach (MemberInfo mi in t.GetMembers(BindingFlags.Public |
                BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly))
                ms.Add(mi.Name);  

            _requestedMembers[t] = ms;
        }

        private void Request(Type t, params string[] members)
        {
            _requestedTypes.Add(t);

            List<string> ms;// = new List<string>();
            if (!_requestedMembers.TryGetValue(t, out ms))
                ms = new List<string>();
            ms.AddRange(members);
            _requestedMembers[t] = ms;
        }

        /// <summary>
        /// Returns true iff the given member 'm' in type 't' has been requested,
        /// either directly or indirectly (i.e. if 'm' is inherited from a 
        /// requested member).
        /// </summary>
        private bool IsMemberRequested(Type targetType, string m)
        {
            List<string> ms;

            foreach (Type t in EnumerateAncestors(targetType))
            {
                // Was the member requested in a supertype?
                if (_requestedMembers.TryGetValue(t, out ms))
                {
                    if (ms.Contains(m))
                        return true;
                }
            }
            return false;
        }

        private void ReadImports(string path)
        {
            List<Assembly> references = new List<Assembly>();
            references.Add(Assembly.GetAssembly(typeof(object)));

            using (StreamReader r = File.OpenText(path))
            {
                string line;
                while ((line = r.ReadLine()) != null)
                {
                    if (line.Trim() == string.Empty) continue;
                    if (line.StartsWith("--") || line.StartsWith("#"))
                        continue;

                    string[] sides = line.Split(new char[] { ':', ' ', '\t' }, 2,
                        StringSplitOptions.RemoveEmptyEntries);


                    if (sides[0] == "reference")
                    {
                        if (sides.Length != 2)
                            throw new Exception("Must specify assembly name after 'reference'");

                        string assemNameOrPath = sides[1];
                        Assembly assem = null;

                        bool failedToLoad = false;

                        try
                        {
                          assem = Assembly.LoadFile(Path.GetFullPath(assemNameOrPath));
                        }
                        catch
                        {
                          failedToLoad = true;
                        }

                        if (failedToLoad)
                          try
                          {
                            failedToLoad = false;
                            assem = Assembly.Load(assemNameOrPath);
                          }
                          catch
                          {
                            failedToLoad = true;
                          }

                        if (failedToLoad)
                          throw new Exception("Failed to load reference: '" + assemNameOrPath + "'");

                        references.Add(assem);

                    }
                    else
                    {
                        string[] members;
                        if (sides.Length > 1)
                            members = sides[1].Split(new char[] { ',', ' ', '\t' },
                                StringSplitOptions.RemoveEmptyEntries);
                        else
                            members = new string[0];

                        bool foundType = false;
                        foreach (Assembly a in references)
                        {
                            Type t = a.GetType(sides[0]);
                            if (t != null)
                            {
                                if (members.Length > 0 && members[0] == "*")
                                    RequestAll(t);
                                else
                                    Request(t, members);
                                foundType = true;
                                break;
                            }
                        }

                        if (!foundType)
                            throw new Exception(string.Format(
                                "The type '{0}' does not exist (are you missing an assembly reference?)",
                                sides[0]));
                    }
                }
            }
        }

        private void Run(string[] args)
        {
            string outputPath = @".";

            foreach (string arg in args)
                ReadImports(arg);

            // Require System.Array (the base class of all arrays), and its
            // CreateInstance method, because the Salsa library uses it to 
            // instantiate arrays.
//            Request(typeof(System.Array), "CreateInstance");

            using (w = File.CreateText(Path.Combine(outputPath, "Bindings.hs")))
            {
                // Explicitly request all supertypes too (since their members are inherited 
                // by the requested types)
                foreach (Type requestedType in Enumerable.ToList(_requestedTypes))
                {
                    foreach (Type superRequestedType in EnumerateAncestors(requestedType))
                    {
                        _requestedTypes.Add(superRequestedType);
                        
                        if (_requestedMembers.ContainsKey(requestedType))
                            Request(superRequestedType, _requestedMembers[requestedType].ToArray());
                    }
                }

                foreach (Type requestedType in _requestedTypes)
                {
                    RequireType(requestedType);
                    if (!_requestedMembers.ContainsKey(requestedType))
                        _requestedMembers.Add(requestedType, new List<string>());
                }

                w.WriteLine("{-# LANGUAGE ForeignFunctionInterface, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, TypeOperators, TypeSynonymInstances, UndecidableInstances, DataKinds #-}");

                w.WriteLine("module {0} (", "Bindings");
                w.WriteLine("  module Labels");
                w.WriteLine("  ) where");
                w.WriteLine();
                w.WriteLine("import Labels");
                w.WriteLine("import Foreign.Salsa.Binding");

                while (_requiredTypes.Count > 0)
                {
                    Type t = _requiredTypes.Pop();
                    if (_generatedTypes.Contains(t)) continue; // Already generated

                    if (IsUnsupportedType(t)) continue;
                    if (IsPrim(t)) continue; // TODO: Support boxed primitive types later.

                    Console.WriteLine("Binding " + t.ToString());
                    WriteClass(t);

                    _generatedTypes.Add(t);
                }

                Console.WriteLine("Generated bindings for {0} classes", _generatedTypes.Count);
            }

            using (w = File.CreateText(Path.Combine(outputPath, "Labels.hs")))
            {
                w.WriteLine("{-# LANGUAGE EmptyDataDecls, FlexibleContexts #-}");

                w.WriteLine("module Labels where");
                w.WriteLine("import Foreign.Salsa (invoke)");
                w.WriteLine();
                WriteLabels();
                WriteInvokers();
            }
        }

        /// <summary>
        /// Indicates that a particular type should be generated (if it hasn't
        /// already been generated).
        /// </summary>
        private void RequireType(Type t)
        {
            if (!_generatedTypes.Contains(t))
                _requiredTypes.Add(t);
        }

        private void WriteClass(Type targetType)
        {
//            if (targetType.IsArray)
//            {
//                // Explicit bindings for array types are not generated by the code generator.
//                // We only need to generate bindings for the element type, the base type
//                // (System.Array) (which is always generated) and the parameterised 'Arr t'
//                // type (see end of this method).
//
//                // Require the element type of the array
//                RequireType(targetType.GetElementType());
//
//                return;
//            }

            string classLabel = TypeToLabel(targetType);

            w.WriteLine();
            w.WriteLine("--");
            w.WriteLine("-- Class: {0}", targetType.FullName);
            w.WriteLine("--");
            w.WriteLine();


            //
            // Write a Salsa.SalsaForeignType instance for this class
            //
            // Note: Type.GetType always returns the same instance for a given type, so it is 
            //       safe to use 'unsafePerformIO' below.
            //
            w.WriteLine("instance SalsaForeignType {0} where", classLabel);
            w.WriteLine("  foreignTypeOf _ = unsafePerformIO $ type_GetType \"{0}\"", targetType.AssemblyQualifiedName);
            w.WriteLine();

            // TODO: Cache result of 'foreignTypeOf' in an IORef?

            {
                //
                // Instance and static methods
                //
                {
                    // Build a list of relevant instance methods for the target class.
                    // Filter out 'override' methods and 'get/set/add/remove' methods.
                    // Also filter out methods with the same name and signature in a base 
                    // class (i.e. hide by signature).
                    List<MemberInfo> relevantMethods = new List<MemberInfo>();

                    // Go down the inheritance tree from object to the target type
                    foreach (Type implementedType in Enumerable.Reverse(EnumerateAncestors(targetType)))
                    {
                        foreach (MethodInfo mi in implementedType.GetMethods(
                            BindingFlags.DeclaredOnly | BindingFlags.Public |
                            BindingFlags.Instance | BindingFlags.Static))
                        {
                            if (mi.IsSpecialName) continue; // Skip get/set/add/remove methods

                            // Skip overridden (virtual but not a new slot) methods
                            if (mi.IsVirtual && (mi.Attributes & MethodAttributes.NewSlot) == 0) continue;

                            // Ignore methods containing parameters of unsupported types, or an 
                            // unsupported result type
                            if (IsUnsupportedType(mi.ReturnType) ||
                                Enumerable.Any(mi.GetParameters(),
                                    delegate(ParameterInfo pi) { return IsUnsupportedType(pi.ParameterType); }))
                                continue;

                            // Remove any methods (from base classes) with this name and signature
                            // (i.e. implement hide-by-signature semantics, like C#)
                            relevantMethods.RemoveAll(delegate(MemberInfo mi2)
                            {
                                return HasSameSignature((MethodInfo)mi, (MethodInfo)mi2);
                            });

                            relevantMethods.Add(mi);
                        }
                    }

                    // Enumerate over the method groups in the relevant instance methods,
                    // generating bindings for each group
                    foreach (IGrouping<string, MemberInfo> mg in Enumerable.GroupBy<MemberInfo, string>(
                        relevantMethods, delegate(MemberInfo mi)
                        {
                            return (((MethodInfo)mi).IsStatic ? "static " : "") + mi.Name;
                        }))
                    {
                        if (!IsMemberRequested(targetType, Enumerable.First(mg).Name)) continue;

                        w.WriteLine("-- " + mg.Key);

                        WriteMethodGroup(mg, targetType);
                    }
                }

                //
                // Constructors
                //
                if (typeof(Delegate).IsAssignableFrom(targetType))
                {
                    // Delegate constructors are special, since they accept a normal 
                    // Haskell function as an argument
                    WriteDelegateConstructor(targetType);
                }
                else
                {
                    // Build a list of relevant constructors for the target class (if any)
                    List<ConstructorInfo> relevantConstructors = new List<ConstructorInfo>();
                    foreach (ConstructorInfo ci in targetType.GetConstructors())
                    {
                        // Ignore methods containing parameters of unsupported types
                        if (Enumerable.Any(ci.GetParameters(),
                            delegate(ParameterInfo pi) { return IsUnsupportedType(pi.ParameterType); }))
                            continue;

                        relevantConstructors.Add(ci);
                    }

                    if (relevantConstructors.Count > 0)
                    {
                        // Generate bindings for the constructors (treated as a single method group)
                        w.WriteLine("-- Constructors");
                        WriteConstructorGroup(relevantConstructors, targetType);
                        w.WriteLine();
                    }
                }

                //
                // Instance and static properties
                //
                {
                    // Build a list accessors for accessable, non-indexed, properties in the target class
                    List<AccessorInfo<PropertyInfo>> relevantPropertyAccessors =
                        new List<AccessorInfo<PropertyInfo>>();

                    // Go down the inheritance tree from object to the target type
                    foreach (Type implementedType in Enumerable.Reverse(EnumerateAncestors(targetType)))
                    {
                        foreach (PropertyInfo pi in implementedType.GetProperties(
                            BindingFlags.DeclaredOnly | BindingFlags.Public |
                            BindingFlags.Instance | BindingFlags.Static))
                        {
                            // Ignore indexed properties (since they're currently out of scope, FIXME)
                            if (pi.GetIndexParameters().Length > 0)
                                continue;

                            // Ignore properties of unsupported types
                            if (IsUnsupportedType(pi.PropertyType)) continue;

                            foreach (MethodInfo mi in pi.GetAccessors())
                            {
                                // Skip static properties declared outside of the target type
                                AccessorInfo<PropertyInfo> ai = new AccessorInfo<PropertyInfo>(
                                    mi == pi.GetGetMethod() ? AccessorType.Get : AccessorType.Set,
                                    pi, mi);

                                // Remove any shadowed properties (from base classes)
                                relevantPropertyAccessors.RemoveAll(
                                    delegate(AccessorInfo<PropertyInfo> ai2)
                                    {
                                        return HasSameSignature(ai.Accessor, ai2.Accessor);
                                    });

                                relevantPropertyAccessors.Add(ai);
                            }
                        }
                    }

                    // Enumerate over the properties and generate attribute bindings for each
                    // pair of get/set accessors
                    foreach (IGrouping<string, AccessorInfo<PropertyInfo>> mg in
                        Enumerable.GroupBy<AccessorInfo<PropertyInfo>, string>(
                            relevantPropertyAccessors,
                            delegate(AccessorInfo<PropertyInfo> ai)
                            {
                                return (ai.Accessor.IsStatic ? "static " : "") + ai.Owner.Name;
                            }))
                    {
                        if (!IsMemberRequested(targetType, Enumerable.First(mg).Owner.Name)) continue;

                        WriteProperty(mg, targetType);
                    }
                }

                //
                // Static and instance events
                //
                {
                    // Build a list accessors for events in the target class
                    List<AccessorInfo<EventInfo>> relevantEventAccessors =
                        new List<AccessorInfo<EventInfo>>();

                    // Go down the inheritance tree from object to the target type
                    foreach (Type implementedType in Enumerable.Reverse(EnumerateAncestors(targetType)))
                    {
                        foreach (EventInfo ei in implementedType.GetEvents(
                            BindingFlags.DeclaredOnly | BindingFlags.Public |
                            BindingFlags.Instance | BindingFlags.Static))
                        {
                            // Ignore events of unsupported types
                            if (IsUnsupportedType(ei.EventHandlerType)) continue;

                            foreach (MethodInfo mi in new MethodInfo[] {
                            ei.GetAddMethod(), ei.GetRemoveMethod() })
                            {
                                // Skip static events declared outside of the target type
                                AccessorInfo<EventInfo> ai = new AccessorInfo<EventInfo>(
                                    mi == ei.GetAddMethod() ? AccessorType.Add : AccessorType.Remove,
                                    ei, mi);

                                // Remove any shadowed events (from base classes)
                                relevantEventAccessors.RemoveAll(
                                    delegate(AccessorInfo<EventInfo> ai2)
                                    {
                                        return HasSameSignature(ai.Accessor, ai2.Accessor);
                                    });

                                relevantEventAccessors.Add(ai);
                            }
                        }
                    }

                    // Enumerate over the events and generate attribute bindings for each
                    // pair of add/remove accessors
                    foreach (IGrouping<string, AccessorInfo<EventInfo>> mg in
                        Enumerable.GroupBy<AccessorInfo<EventInfo>, string>(
                            relevantEventAccessors,
                            delegate(AccessorInfo<EventInfo> ai)
                            {
                                return (ai.Accessor.IsStatic ? "static " : "") + ai.Owner.Name;
                            }))
                    {
                        if (!IsMemberRequested(targetType, Enumerable.First(mg).Owner.Name)) continue;

                        WriteEvent(mg, targetType);
                    }
                }

                //
                // Instance and static fields
                //
                {
                    // Build a list accessible fields in the target class
                    List<FieldInfo> relevantFields = new List<FieldInfo>();

                    // Go down the inheritance tree from object to the target type
                    foreach (Type implementedType in Enumerable.Reverse(EnumerateAncestors(targetType)))
                    {
                        foreach (FieldInfo fi in implementedType.GetFields(
                            BindingFlags.DeclaredOnly | BindingFlags.Public |
                            BindingFlags.Instance | BindingFlags.Static))
                        {
                            // Ignore fields of unsupported types
                            if (IsUnsupportedType(fi.FieldType)) continue;

                            // Remove any shadowed fields (from base classes)
                            relevantFields.RemoveAll(
                                delegate(FieldInfo fi2)
                                {
                                    return fi.Name == fi2.Name;
                                });

                            relevantFields.Add(fi);
                        }
                    }

                    // Enumerate over the fields and generate bindings for each
                    foreach (FieldInfo fi in relevantFields)
                    {
                        if (!IsMemberRequested(targetType, fi.Name)) continue;

                        WriteField(fi, targetType);
                    }
                }
            }

            //
            // Ancestors
            //

            List<Type> supertypes = new List<Type>();

            foreach (Type t in EnumerateAncestors(targetType))
                supertypes.Add(t);

            supertypes.AddRange(targetType.GetInterfaces());

            // Remove any unsupported types from 'supertypes'
            foreach (Type t in Enumerable.ToList(supertypes))
            {
                if (IsUnsupportedType(t))
                    supertypes.Remove(t);
            }

            w.WriteLine("type instance SupertypesOf {0} = {1}",
                ToHaskellType(targetType),
                Util.JoinSuffix(" ': ", Enumerable.Select<Type, string>(supertypes,
                    delegate(Type t) { return ToHaskellType(t); }), "'[]"));

            foreach (Type supertype in supertypes)
                RequireType(supertype);

//            if (targetType == typeof(System.Array))
//            {
//                //
//                // Delegate the members of any array type (i.e. any 'Arr t') to System.Array
//                //
//                w.WriteLine();
//                w.WriteLine("--");
//                w.WriteLine("-- 'Arr t' to 'System.Array' delegation code");
//                w.WriteLine("--");
//                w.WriteLine();
//                Console.WriteLine(string.Join(", ", Assembly.GetExecutingAssembly().GetManifestResourceNames()));
//                using (StreamReader r = new StreamReader(
//                    Assembly.GetExecutingAssembly().GetManifestResourceStream(typeof(Generator), "Array.hs")))
//                {
//                    string line;
//                    while ((line = r.ReadLine()) != null)
//                        w.WriteLine(line);
//                }
//            }
        }

        /// <summary>
        /// Generates bindings for a delegate constructor for the given delegate type.
        /// </summary>
        private void WriteDelegateConstructor(Type delegateType)
        {
            string classLabel = TypeToLabel(delegateType);

            w.WriteLine();
            w.WriteLine("--");
            w.WriteLine("-- Delegate: {0}", delegateType.Name);
            w.WriteLine("--");
            w.WriteLine();

            if (delegateType != typeof(Delegate) &&
                delegateType != typeof(MulticastDelegate))
            {
                ConstructorInfo constructorCi = delegateType.GetConstructors()[0];
                MethodInfo invokerMi = delegateType.GetMethod("Invoke");

                string target = TypeToLabel(delegateType);

                string stubFunction = ToStubName(constructorCi);
                string wrapperFunction = "wrap_" + stubFunction;
                string wrapperType = "Type_" + wrapperFunction;

                ParameterInfo[] parameters = GetParameters(invokerMi);

                // Generate the base type signature for Haskell implementation of the delegate
                w.WriteLine("type {0} = {1}", wrapperType,
                    Util.JoinSuffix(" -> ", Enumerable.Select<ParameterInfo, string>(parameters,
                        delegate(ParameterInfo pi) { return ToBaseType(pi.ParameterType); }),
                        ToBaseReturnType(GetMemberReturnType(invokerMi))));

                w.WriteLine("foreign import ccall \"wrapper\" {0} :: {1} -> (IO (FunPtr {1}))",
                    wrapperFunction, wrapperType);
                w.WriteLine();

                // Generate the delegate-object-generating stub function
                w.WriteLine("{{-# NOINLINE {0} #-}}", stubFunction);
                w.WriteLine("{0} :: {1} -> IO ObjectId", stubFunction, wrapperType);
                w.WriteLine("{0} = unsafePerformIO $ getDelegateConstructorStub", stubFunction);
                w.WriteLine("    \"{0}\"", delegateType.AssemblyQualifiedName);
                w.WriteLine("    {0}", wrapperFunction);
                w.WriteLine();

                // Generate the 'delegate' instance for calling the delegate constructor
                w.WriteLine("instance Delegate {0} where", target);
                w.WriteLine("    type DelegateT {0} = {1}", target,
                    Util.JoinSuffix(" -> ", Enumerable.Select<ParameterInfo, string>(parameters,
                        delegate(ParameterInfo pi) { return ToHaskellType(pi.ParameterType); }),
                        ToReturnType(GetMemberReturnType(invokerMi))));
                w.WriteLine("    delegate _ handler = {0} (marshalFn{1} handler) >>= unmarshal",
                    stubFunction, parameters.Length);
                w.WriteLine();
            }

            w.WriteLine();
        }
        
        /// <summary>
        /// Generates bindings for a method group containing: all instance methods,
        /// all static methods, or all instance constructors.
        /// </summary>
        private void WriteMethodGroup(IEnumerable<MemberInfo> members, Type forType)
        {
            MemberInfo firstMi = Enumerable.First(members);

            Console.WriteLine("  {0}", firstMi.Name);

            ParameterInfo[] parameters = GetParameters(firstMi);
            Type returnType = GetMemberReturnType(firstMi);
            bool isStatic = IsMemberStatic(firstMi);

            string label = GetMemberLabel(firstMi);
            string target = isStatic ? TypeToLabel(forType) : ToHaskellType(forType);

            // Output the parameter lists for the members of the method group
            w.WriteLine("type instance Candidates {0} {1} = {2}",
                target, label,
                Util.JoinSuffix(" ': ", Enumerable.Select<MemberInfo, string>(members,
                    delegate(MemberInfo mi) { return ToListType(GetParameters(mi)); }), "'[]"));

            // Output instances for invoking each method group member
            foreach (MemberInfo mi in members)
                WriteMethod(mi, forType);
            w.WriteLine();

            // Require the return type of this method
            RequireType(returnType);
        }

        /// <summary>
        /// Generates bindings for a group of instance constructors.
        /// </summary>
        private void WriteConstructorGroup(IEnumerable<ConstructorInfo> members, Type forType)
        {
            ConstructorInfo firstCi = Enumerable.First(members);
            string target = TypeToLabel(forType);
            string label = "Ctor";

            // Output the parameter lists for the constructors
            w.WriteLine("type instance Candidates {0} {1} = {2}",
                target, label,
                Util.JoinSuffix(" ': ", Enumerable.Select<ConstructorInfo, string>(members,
                    delegate(ConstructorInfo ci) { return ToListType(ci.GetParameters()); }), "'[]"));

            // Output instances for invoking each constructor
            foreach (ConstructorInfo ci in members)
                WriteMethod(ci, forType);
        }

        /// <summary>
        /// Generate bindings for a static method, instance method, or instance constructor,
        /// for use in a particular class (which, in the case of an instance method, may be a
        /// descendant of the class in which the method was declared).
        /// </summary>
        private void WriteMethod(MemberInfo mi, Type forType)
        {
            // If the method was declared on the target type (forType), then produce
            // the FFI stub function (which is called by Invoker instances down the
            // hierarchy)
            if (mi.DeclaringType == forType)
                WriteMethodStub(mi);

            // Invoker instance:
            w.WriteLine("instance Invoker {0} {1} {2} where",
                IsMemberStatic(mi) ? TypeToLabel(forType) : ToHaskellType(forType),
                GetMemberLabel(mi), ToTupleType(GetParameters(mi)));
            w.WriteLine("  type Result {0} {1} {2} = {3}",
                IsMemberStatic(mi) ? TypeToLabel(forType) : ToHaskellType(forType),
                GetMemberLabel(mi), ToTupleType(GetParameters(mi)),
                ToReturnType(GetMemberReturnType(mi)));
            w.WriteLine("  rawInvoke = {0}", ToMethodMarshaler(mi));

            // Require any parameter types for this method/constructor
            foreach (ParameterInfo pi in GetParameters(mi))
                RequireType(pi.ParameterType);
        }
        
        /// <summary>
        /// Returns true iff the given member should be treated as a static member.
        /// Static methods, static properties, and instance constructors are all
        /// treated as static members.
        /// </summary>
        private bool IsMemberStatic(MemberInfo mi)
        {
            if (mi is ConstructorInfo)
                return true;
            if (mi is MethodInfo)
                return ((MethodInfo)mi).IsStatic;
            if (mi is FieldInfo)
                return ((FieldInfo)mi).IsStatic;
            throw new ArgumentException("Expected a ConstructorInfo, MethodInfo or FieldInfo.");
        }

        /// <summary>
        /// Returns the Haskell code for a function that calls the given method, 
        /// constructor, or property accessor, marshaling the arguments and result 
        /// value as necessary.
        /// </summary>
        private string ToMethodMarshaler(MemberInfo mi)
        {
            return string.Format("marshalMethod{0}{1} {2}",
                GetParameters(mi).Length, IsMemberStatic(mi) ? "s" : "i",
                ToStubName(mi));
        }

        /// <summary>
        /// Generates an FFI stub function for calling a particular .NET method
        /// or property accessor.
        /// </summary>
        private void WriteMethodStub(MemberInfo mi)
        {
            // TODO: Perhaps use unboxed string literals?

            string stubFunction = ToStubName(mi);
            string stubType = "Type_" + stubFunction;
            string makeFunction = "make_" + stubFunction;

            w.WriteLine();
            w.WriteLine("-- Foreign Interface Stub for {0}.{1}:",
                mi.DeclaringType.Name, mi.Name);
            w.WriteLine("type {0} = {1}{2}", stubType,
                IsMemberStatic(mi) ? "" : (ToBaseType(mi.DeclaringType) + " -> "),
                Util.JoinSuffix(" -> ", Enumerable.Select<ParameterInfo, string>(GetParameters(mi),
                    delegate(ParameterInfo pi) { return ToBaseType(pi.ParameterType); }),
                    ToBaseReturnType(GetMemberReturnType(mi))));
            w.WriteLine("foreign import ccall \"dynamic\" {0} :: FunPtr {1} -> {1}",
                makeFunction, stubType);
            w.WriteLine();
            w.WriteLine("{{-# NOINLINE {0} #-}}", stubFunction);
            w.WriteLine("{0} :: {1}", stubFunction, stubType);
            w.WriteLine("{0} = {1} $ unsafePerformIO $ getMethodStub", stubFunction, makeFunction);
            w.WriteLine("    \"{0}\" \"{1}\"", mi.DeclaringType.AssemblyQualifiedName, mi.Name);
            w.WriteLine("    \"{0}\"",
                Util.Join(";", Enumerable.Select<ParameterInfo, string>(GetParameters(mi),
                    delegate(ParameterInfo pi) { return ToQualifiedType(pi.ParameterType); })));
            w.WriteLine();
        }

        /// <summary>
        /// Generates FFI stub functions for retrieving or setting a particular .NET field.
        /// </summary>
        private void WriteFieldStub(FieldInfo fi, AccessorType accessorType)
        {
            bool isGet = (accessorType == AccessorType.Get);
            string stubFunction = ToStubName(fi) + "_" + (isGet ? "get" : "set");
            string stubType = "Type_" + stubFunction;
            string makeFunction = "make_" + stubFunction;

            w.WriteLine();
            w.WriteLine("-- Field accessor stub for {0}.{1}:", fi.DeclaringType.Name, fi.Name);
            w.WriteLine("type {0} = {1}{2}{3}", 
                stubType,
                fi.IsStatic ? "" : (ToBaseType(fi.DeclaringType) + " -> "),
                isGet ? "" : (ToBaseType(fi.FieldType) + " -> "),
                isGet ? ToBaseReturnType(fi.FieldType) : "IO ()");

            w.WriteLine("foreign import ccall \"dynamic\" {0} :: FunPtr {1} -> {1}",
                makeFunction, stubType);

            w.WriteLine();
            w.WriteLine("{{-# NOINLINE {0} #-}}", stubFunction);
            w.WriteLine("{0} :: {1}", stubFunction, stubType);
            w.WriteLine("{0} = {1} $ unsafePerformIO $ {2}", stubFunction, makeFunction,
                isGet ? "getFieldGetStub" : "getFieldSetStub");
            w.WriteLine("    \"{0}\" \"{1}\"", fi.DeclaringType.AssemblyQualifiedName, fi.Name);
            w.WriteLine();
        }

        private void WriteProperty(IEnumerable<AccessorInfo<PropertyInfo>> accessors, Type forType)
        {
            AccessorInfo<PropertyInfo> firstAccessor = Enumerable.First(accessors);
            bool isStatic = IsMemberStatic(firstAccessor.Accessor);

            string target = isStatic ? TypeToLabel(forType) : ToHaskellType(forType);
            string label = ToLabelType(firstAccessor.Owner.Name);

            w.WriteLine("instance Prop {0} {1} where", target, label);

            AccessorInfo<PropertyInfo> getAccessor = Enumerable.FirstOrDefault(accessors,
                delegate(AccessorInfo<PropertyInfo> ai) { return ai.Type == AccessorType.Get; });
            AccessorInfo<PropertyInfo> setAccessor = Enumerable.FirstOrDefault(accessors,
                delegate(AccessorInfo<PropertyInfo> ai) { return ai.Type == AccessorType.Set; });

            if (getAccessor != null)
            {
                w.WriteLine("    type PropGT {0} {1} = {2}", target, label,
                    ToHaskellType(getAccessor.Owner.PropertyType));
                w.WriteLine("    getProp t pn = {0} t pn ()", ToMethodMarshaler(getAccessor.Accessor));
            }
            else
            {
                w.WriteLine("    type PropGT {0} {1} = ()", target, label);
                w.WriteLine("    getProp _ _ = return ()"); // Return nothing
            }

            if (setAccessor != null)
            {
                w.WriteLine("    type PropST {0} {1} = {2}", target, label,
                    ToHaskellType(setAccessor.Owner.PropertyType));
                w.WriteLine("    setProp = {0}", ToMethodMarshaler(setAccessor.Accessor));
            }
            else
            {
                w.WriteLine("    type PropST {0} {1} = ()", target, label);
                w.WriteLine("    setProp _ _ _ = return ()"); // Do nothing
            }
            
            foreach (AccessorInfo<PropertyInfo> accessor in accessors)
            {
                // Generate the get/set stub caller (in declared type only)
                if (accessor.Accessor.DeclaringType == forType)
                    WriteMethodStub(accessor.Accessor);
            }

            w.WriteLine();
        }

        private void WriteEvent(IEnumerable<AccessorInfo<EventInfo>> accessors, Type forType)
        {
            AccessorInfo<EventInfo> firstAccessor = Enumerable.First(accessors);
            bool isStatic = IsMemberStatic(firstAccessor.Accessor);

            string target = isStatic ? TypeToLabel(forType) : ToHaskellType(forType);
            string label = ToLabelType(firstAccessor.Owner.Name);

            w.WriteLine("instance Event {0} {1} where", target, label);
            w.WriteLine("    type EventT {0} {1} = {2}", target, label,
                ToHaskellType(firstAccessor.Owner.EventHandlerType));

            foreach (AccessorInfo<EventInfo> accessor in accessors)
            {
                if (accessor.Type == AccessorType.Add)
                    w.WriteLine("    addEvent    = {0}", ToMethodMarshaler(accessor.Accessor));
                else // AccessorType.Remove
                    w.WriteLine("    removeEvent = {0}", ToMethodMarshaler(accessor.Accessor));
            }

            foreach (AccessorInfo<EventInfo> accessor in accessors)
            {
                // Generate the add/remove stub caller (in declared type only)
                if (accessor.Accessor.DeclaringType == forType)
                    WriteMethodStub(accessor.Accessor);
            }

            w.WriteLine();

            // Require the event type of this event
            RequireType(firstAccessor.Owner.EventHandlerType);
        }

        private void WriteField(FieldInfo field, Type forType)
        {
            bool isStatic = field.IsStatic;
            bool isReadOnly = field.IsLiteral || field.IsInitOnly;

            // TODO: Add support for literal fields.  The constant value from
            //       the metadata should be included in the generated code.

            string target = isStatic ? TypeToLabel(forType) : ToHaskellType(forType);
            string label = ToLabelType(field.Name);

            w.WriteLine("instance Prop {0} {1} where", target, label);

            w.WriteLine("    type PropGT {0} {1} = {2}", target, label, ToHaskellType(field.FieldType));
            w.WriteLine("    getProp t pn = marshalMethod0{0} {1}_get t pn ()",
                isStatic ? "s" : "i", ToStubName(field));

            if (!isReadOnly)
            {
                w.WriteLine("    type PropST {0} {1} = {2}", target, label, ToHaskellType(field.FieldType));
                w.WriteLine("    setProp = marshalMethod1{0} {1}_set", 
                    isStatic ? "s" : "i", ToStubName(field));
            }
            else
            {
                w.WriteLine("    type PropST {0} {1} = ()", target, label);
                w.WriteLine("    setProp _ _ _ = return ()"); // Do nothing
            }

            // Generate the get/set stub caller (in declared type only)
            if (field.DeclaringType == forType)
            {
                if (!isReadOnly)
                    WriteFieldStub(field, AccessorType.Set);
                WriteFieldStub(field, AccessorType.Get);
            }

            // For readonly fields, generate an IO-less method getting the value
            if (isReadOnly)
            {
                // Ensure that an invoker is generated for invoking the label with '#'
                ToInvoker(field.Name);

                w.WriteLine("type instance Candidates {0} {1} = '[]", target, label);

                w.WriteLine("instance Invoker {0} {1} () where", target, label);
                w.WriteLine("  type Result {0} {1} () = {2}", target, label, ToHaskellType(field.FieldType));
                w.WriteLine("  rawInvoke t m () = unsafePerformIO $ get t m ");
            }

            w.WriteLine();
        }

        private string ToHaskellType(Type t)
        {
            if (t == typeof(void)) return "()";
            if (t == typeof(int)) return "Int32";
            if (t == typeof(string)) return "String";
            if (t == typeof(bool)) return "Bool";
            if (t == typeof(Double)) return "Double";
            if (t == typeof(bool?)) return "(Maybe Bool)";
//            if (t.IsArray) 
//                return string.Format("(Obj (Arr {0}))", ToHaskellType(t.GetElementType()));
            return "(Obj " + TypeToLabel(t) + ")";
        }

        private string ToReturnType(Type t)
        {
            return "IO " + ToHaskellType(t);
        }

        /// <summary>
        /// Gives the low-level Haskell type for the given .NET type 't'.  This is the
        /// base type used in FFI declarations.
        /// </summary>
        private string ToBaseType(Type t)
        {
            if (t == typeof(void)) return "()";
            if (t == typeof(Int32)) return "Int32";
            if (t == typeof(String)) return "SalsaString";
            if (t == typeof(Boolean)) return "Bool";
            if (t == typeof(Double)) return "Double";
            return "ObjectId";
        }

        private string ToBaseReturnType(Type t)
        {
            return "IO " + ToBaseType(t);
        }

        private bool IsPrim(Type t)
        {
            return
                t == typeof(void) ||
                t == typeof(Int32) ||
                t == typeof(String) ||
                t == typeof(Boolean) ||
                t == typeof(Double);
        }

        private bool IsUnsupportedType(Type t)
        {
            return
                t.Name.StartsWith("_") ||
                t.IsByRef ||        // FIXME: Support this?
                t.IsPointer ||
                // Salsa doesn't support generic types yet, but there's a special case for Nullable<bool>
                (t.IsGenericType && t != typeof(Nullable<bool>)) ||
                t.IsGenericParameter ||
                t.IsArray; // Temporarily removed array support
//                // Salsa only supports arrays of non-generic types at present
//                (t.IsArray && IsUnsupportedType(t.GetElementType()));
        }

        /// <summary>
        /// Returns a tuple of high-level Haskell types corresponding to the given
        /// list of method parameters.
        /// </summary>
        private string ToTupleType(ParameterInfo[] ts)
        {
            return "(" + Util.Join(", ", Enumerable.Select<ParameterInfo, string>(ts,
                delegate(ParameterInfo pi) { return ToHaskellType(pi.ParameterType); } )) + ")";
        }

        /// <summary>
        /// Returns a type-level list of high-level Haskell types corresponding to 
        /// the given list of method parameters.
        /// </summary>
        private string ToListType(ParameterInfo[] ts)
        {
            return "(" + Util.JoinSuffix(" ': ", Enumerable.Select<ParameterInfo, string>(ts,
                delegate(ParameterInfo pi) { return ToHaskellType(pi.ParameterType); } ), "'[]") + ")";
        }

        /// <summary>
        /// Returns the name of the raw FFI stub method associated with the given .NET method
        /// or constructor.  There is a unique stub name for every declared method overload,
        /// and constructor.
        /// </summary>
        private string ToStubName(MemberInfo mi)
        {
            string parameterDetails = "";
            if (mi is ConstructorInfo || mi is MethodInfo)
                parameterDetails = Util.Join(" ", Enumerable.Select<ParameterInfo, string>(GetParameters(mi),
                        delegate(ParameterInfo pi) { return pi.ParameterType.AssemblyQualifiedName; }));

            return string.Format("stub_{0}",
                GetUnique("stub",
                    mi.DeclaringType.AssemblyQualifiedName,
                    mi.Name,
                    parameterDetails));
        }

        /// <summary>
        /// Returns the name of the FFI wrapper function that is used to wrap Haskell 
        /// functions that implement the given .NET delegate type.
        /// </summary>
        private string ToWrapperName(Type dt)
        {
            return string.Format("wrap_{0}",
                GetUnique("wrap", dt.AssemblyQualifiedName));
        }

        /// <summary>
        /// Returns a string that uniquely identifies the given type.  It is 
        /// used to retrieve stub functions for the particular type at runtime.
        /// </summary>
        private string ToQualifiedType(Type t)
        {
            // Unless the type is in mscorlib, use the (long) assembly qualified name
            if (t.Assembly == typeof(object).Assembly)
                return t.FullName;
            else
                return t.AssemblyQualifiedName;
        }

        private ParameterInfo[] GetParameters(MemberInfo mi)
        {
            if (mi is MethodInfo)
                return ((MethodInfo)mi).GetParameters();
            if (mi is ConstructorInfo)
                return ((ConstructorInfo)mi).GetParameters();
            throw new ArgumentException("Must be a MethodInfo or ConstructorInfo.", "mi");
        }

        private Type GetMemberReturnType(MemberInfo mi)
        {
            if (mi is MethodInfo)
                return ((MethodInfo)mi).ReturnType;
            if (mi is ConstructorInfo)
                return ((ConstructorInfo)mi).DeclaringType;
            throw new ArgumentException("Must be a MethodInfo or ConstructorInfo.", "mi");
        }

        private string GetMemberLabel(MemberInfo mi)
        {
            if (mi is MethodInfo)
                return MethodToLabel((MethodInfo)mi);
            if (mi is ConstructorInfo)
                return "Ctor";
            throw new ArgumentException("Must be a MethodInfo or ConstructorInfo.", "mi");
        }

        /// <summary>
        /// Returns true iff the given methods have the same name, number and type of 
        /// parameters.
        /// </summary>
        private bool HasSameSignature(MethodInfo mi1, MethodInfo mi2)
        {
            if (mi1.Name != mi2.Name) return false;
            ParameterInfo[] pi1 = mi1.GetParameters();
            ParameterInfo[] pi2 = mi2.GetParameters();
            if (pi1.Length != pi2.Length) return false;
            for (int i = 0; i < pi1.Length; i++)
            {
                if (pi1[i].ParameterType != pi2[i].ParameterType)
                    return false;
            }
            return true;
        }

        /// <summary>
        /// Returns an irrefutable Haskell pattern for matching a value of the given type.
        /// 'index' is included in the identified used (if any).
        /// </summary>
        private string ToPattern(Type t, int index)
        {
            if (t == typeof(void)) return "()";
            if (t == typeof(int) || t == typeof(bool) || t == typeof(string))
                return string.Format("a{0}", index);
            return string.Format("(Obj a{0})", index);
        }

        /// <summary>
        /// Enumerate ancestors of 't', starting with 't' and finishing with object.
        /// </summary>
        private IEnumerable<Type> EnumerateAncestors(Type t)
        {
            while (t != null)
            {
                yield return t;
                t = t.BaseType;
            }
        }

        private string MethodToLabel(MethodInfo mi)
        {
            ToInvoker(mi.Name);
            return ToLabelType(mi.Name);
        }

        private string TypeToLabel(Type t)
        {
            if (IsUnsupportedType(t)) return ToLabelType("NotSupported"); 

            if (t.IsNested && t.DeclaringType != null)
            {
                // FIXME: This only handles one level of nested classes 
                //        (and it doesn't handle nested generic types)
                return ToLabelType(t.DeclaringType.Name + "_" + t.Name);
            }
            else if (t.IsGenericType && t.GetGenericTypeDefinition() == typeof(Nullable<>))
            {
                return string.Format("(Maybe {0})", TypeToLabel(t.GetGenericArguments()[0]));
            }
            else
                return ToLabelType(t.Name);
        }

        private string ToLabelHelper(string s)
        {
            if (string.IsNullOrEmpty(s))
                throw new ArgumentException("Expected non-empty string.");
            s = char.ToUpper(s[0]) + s.Substring(1);
            if (!_labels.Contains(s) && 
                s != "Object" && s != "Type" && s != "Array") // These labels are defined in the Salsa library
                _labels.Add(s);
            return s;
        }

        private string ToLabelValue(string s)
        {
            return "_" + ToLabelHelper(s);
        }

        private string ToLabelType(string s)
        {
            return ToLabelHelper(s) + "_";
        }

        private void WriteLabels()
        {
            w.WriteLine("-- Labels for .NET types, methods, properties, fields and events");
            foreach (string label in _labels)
                w.WriteLine("data {0,-25}", ToLabelType(label));
            w.WriteLine();
            foreach (string label in _labels)
                w.WriteLine("{0,-30} = undefined :: {1}", ToLabelValue(label), ToLabelType(label));
            w.WriteLine();
        }

        private string ToInvoker(string s)
        {
            s = Util.ToLowerFirst(s);
            if (!_invokers.Contains(s))
                _invokers.Add(s);
            return "_" + s;
        }

        private void WriteInvokers()
        {
            w.WriteLine("-- Functions for invoking methods with '#'");
            foreach (string invoker in _invokers)
            {
                // Output an invoker (and a unit invoker, for convenience and consistency)
                w.WriteLine("{0} args target = invoke target {1} args", ToInvoker(invoker), ToLabelValue(invoker));
                w.WriteLine("{0}_ target = invoke target {1} ()", ToInvoker(invoker), ToLabelValue(invoker));
            }
            w.WriteLine();
        }
    }

    public static class Util
    {
        public static string Join(string separator, IEnumerable<string> xs)
        {
            StringBuilder sb = new StringBuilder();
            foreach (string x in xs)
            {
                sb.Append(x);
                sb.Append(separator);
            }
            if (sb.Length > 0) sb.Length -= separator.Length;
            return sb.ToString();
        }

        public static string JoinSuffix(string separator, IEnumerable<string> xs, string end)
        {
            StringBuilder sb = new StringBuilder();
            foreach (string x in xs)
            {
                sb.Append(x);
                sb.Append(separator);
            }
            sb.Append(end);
            return sb.ToString();
        }

        public static string ToLowerFirst(string s)
        {
            if (s == "") return "";
            return s.Substring(0, 1).ToLower() + s.Substring(1);
        }

        public static string ToUpperFirst(string s)
        {
            if (s == "") return "";
            return s.Substring(0, 1).ToUpper() + s.Substring(1);
        }
    }

    public class AccessorInfo<T> where T : MemberInfo
    {
        private AccessorType _type;
        private T _owner;
        private MethodInfo _accessor;

        public AccessorInfo(AccessorType type, T owner, MethodInfo accessor)
        {
            _type = type;
            _owner = owner;
            _accessor = accessor;
        }

        public AccessorType Type
        {
            get { return _type; }
        }

        public MethodInfo Accessor
        {
            get { return _accessor; }
        }

        public T Owner
        {
            get { return _owner; }
        }
    }

    public enum AccessorType { Get, Set, Add, Remove };
}
