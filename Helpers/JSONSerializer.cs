﻿using System.Runtime.Serialization.Json;
using System.Text;

namespace Plastic.Helpers
{
    /// <summary>
    /// From: https://stackoverflow.com/questions/9573119/how-to-parse-json-without-json-net-library
    /// </summary>
    /// <typeparam name="TType"></typeparam>
    public static class JSONSerializer<TType> where TType : class
    {
        /// <summary>
        /// Serializes an object to JSON
        /// </summary>
        public static string Serialize(TType instance)
        {
            var serializer = new DataContractJsonSerializer(typeof(TType));
            using (var stream = new MemoryStream())
            {
                serializer.WriteObject(stream, instance);
                return Encoding.Default.GetString(stream.ToArray());
            }
        }

        /// <summary>
        /// DeSerializes an object from JSON
        /// </summary>
        public static TType DeSerialize(string json)
        {
            using (var stream = new MemoryStream(Encoding.Default.GetBytes(json)))
            {
                var serializer = new DataContractJsonSerializer(typeof(TType));
                return serializer.ReadObject(stream) as TType;
            }
        }
    }
}
