//
// Salsa Binding Generator
//
// Licence:   BSD3 (see LICENSE)
//

using System;
using System.Collections.Generic;
using System.Text;

namespace Generator
{
    public class Set<T> : IEnumerable<T>
    {
        private Dictionary<T, object> _store = new Dictionary<T, object>();

        public void Add(T v)
        {
            if (!_store.ContainsKey(v))
                _store.Add(v, null);
        }

        public int Count
        {
            get { return _store.Count; }
        }

        public bool Contains(T v)
        {
            return _store.ContainsKey(v);
        }

        /// <summary>
        /// Removes an item from the set and returns it.
        /// </summary>
        public T Pop()
        {
            foreach (T v in _store.Keys)
            {
                _store.Remove(v);
                return v;
            }
            throw new InvalidOperationException("Set is empty.");
        }

        #region IEnumerable<T> implementation

        public IEnumerator<T> GetEnumerator()
        {
            return _store.Keys.GetEnumerator();
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return _store.Keys.GetEnumerator();
        }

        #endregion
    }
}
