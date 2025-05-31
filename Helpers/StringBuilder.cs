namespace Plastic.Helpers
{
    public class StringBuilder
    {
        public int Length => _length;
        private char[] _buffer;
        private int _length;

        public StringBuilder(int capacity = 256)
        {
            _buffer = new char[capacity];
            _length = 0;
        }

        public StringBuilder Append(object value)
        {
            if (value == null) return this;

            string stringValue = value.ToString(); // Convert object to string  
            EnsureCapacity(_length + stringValue.Length);

            for (int i = 0; i < stringValue.Length; i++)
            {
                _buffer[_length++] = stringValue[i]; // Use string indexing  
            }

            return this;
        }

        public StringBuilder AppendLine(string value = "")
        {
            Append(value);
            Append(System.Environment.NewLine);
            return this;
        }

        public override string ToString()
        {
            return new string(_buffer, 0, _length);
        }

        private void EnsureCapacity(int size)
        {
            if (size > _buffer.Length)
            {
                int newCapacity = _buffer.Length * 2;
                while (newCapacity < size)
                    newCapacity *= 2;

                var newBuffer = new char[newCapacity];
                _buffer.CopyTo(newBuffer, 0);
                _buffer = newBuffer;
            }
        }
    }
}
