﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Plastic.Helpers
{
    public class SHA256
    {
        private static readonly uint[] K = new uint[64]
        {
            0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
            0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
            0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
            0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
            0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
            0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
            0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
            0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
            0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
            0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
            0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
            0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
            0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
            0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
            0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
            0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
        };

        private static uint ROTR(uint x, int n) => (x >> n) | (x << (32 - n));
        private static uint SHR(uint x, int n) => x >> n;

        private static uint Ch(uint x, uint y, uint z) => (x & y) ^ (~x & z);
        private static uint Maj(uint x, uint y, uint z) => (x & y) ^ (x & z) ^ (y & z);
        private static uint Sigma0(uint x) => ROTR(x, 2) ^ ROTR(x, 13) ^ ROTR(x, 22);
        private static uint Sigma1(uint x) => ROTR(x, 6) ^ ROTR(x, 11) ^ ROTR(x, 25);
        private static uint sigma0(uint x) => ROTR(x, 7) ^ ROTR(x, 18) ^ SHR(x, 3);
        private static uint sigma1(uint x) => ROTR(x, 17) ^ ROTR(x, 19) ^ SHR(x, 10);

        public static byte[] ComputeHash(byte[] data)
        {
            uint[] H = new uint[8]
            {
            0x6a09e667,
            0xbb67ae85,
            0x3c6ef372,
            0xa54ff53a,
            0x510e527f,
            0x9b05688c,
            0x1f83d9ab,
            0x5be0cd19
            };

            byte[] padded = Pad(data);
            int blocks = padded.Length / 64;

            for (int i = 0; i < blocks; i++)
            {
                uint[] W = new uint[64];
                for (int t = 0; t < 16; ++t)
                {
                    int index = i * 64 + t * 4;
                    W[t] = ((uint)padded[index] << 24)
                         | ((uint)padded[index + 1] << 16)
                         | ((uint)padded[index + 2] << 8)
                         | ((uint)padded[index + 3]);
                }

                for (int t = 16; t < 64; ++t)
                {
                    W[t] = sigma1(W[t - 2]) + W[t - 7] + sigma0(W[t - 15]) + W[t - 16];
                }

                uint a = H[0], b = H[1], c = H[2], d = H[3];
                uint e = H[4], f = H[5], g = H[6], h = H[7];

                for (int t = 0; t < 64; ++t)
                {
                    uint T1 = h + Sigma1(e) + Ch(e, f, g) + K[t] + W[t];
                    uint T2 = Sigma0(a) + Maj(a, b, c);
                    h = g;
                    g = f;
                    f = e;
                    e = d + T1;
                    d = c;
                    c = b;
                    b = a;
                    a = T1 + T2;
                }

                H[0] += a; H[1] += b; H[2] += c; H[3] += d;
                H[4] += e; H[5] += f; H[6] += g; H[7] += h;
            }

            byte[] result = new byte[32];
            for (int i = 0; i < 8; i++)
            {
                result[i * 4 + 0] = (byte)((H[i] >> 24) & 0xff);
                result[i * 4 + 1] = (byte)((H[i] >> 16) & 0xff);
                result[i * 4 + 2] = (byte)((H[i] >> 8) & 0xff);
                result[i * 4 + 3] = (byte)((H[i]) & 0xff);
            }

            return result;
        }

        private static byte[] Pad(byte[] data)
        {
            ulong bitLength = (ulong)data.Length * 8;
            int paddingLength = 64 - (int)((bitLength / 8 + 9) % 64);
            int totalLength = data.Length + 1 + paddingLength + 8;

            byte[] padded = new byte[totalLength];
            Buffer.BlockCopy(data, 0, padded, 0, data.Length);

            padded[data.Length] = 0x80;
            for (int i = 0; i < 8; i++)
            {
                padded[totalLength - 1 - i] = (byte)((bitLength >> (8 * i)) & 0xff);
            }

            return padded;
        }

        public static string HashString(string input)
        {
            byte[] bytes = Encoding.UTF8.GetBytes(input);
            byte[] hash = ComputeHash(bytes);
            return BitConverter.ToString(hash).Replace("-", "").ToLower();
        }
    }
}
