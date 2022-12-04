using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;

namespace AtCoderProblemsCSharp {
    class D156 {
        static readonly long p = Convert.ToInt64(Math.Pow(10, 9)) + 7L;

        static void _D156(string[] args) {

            var tmp = Console.ReadLine().Split();
            var n = Convert.ToInt32(tmp[0]);
            var a = Convert.ToInt32(tmp[1]);
            var b = Convert.ToInt32(tmp[2]);

            long ans =
                Util.Mod(
                    Util.Power(2, n, p)
                    - Util.Combination(n, a, p)
                    - Util.Combination(n, b, p)
                    - 1,
                    p);

            Console.WriteLine(ans.ToString());

        }
    }

    public static class Util {

        public static long Mod(long x, long p) {
            long tmp = x % p;
            while (tmp < 0) {
                tmp = tmp + p;
            }
            return tmp;
        }

        /// <summary>
        /// メモ化
        /// </summary>
        public static Func<T1, T2> Memoize1<T1, T2>(Func<T1, T2> func) {
            var dic = new Dictionary<T1, T2>();

            return (T1 x) => {
                T2 y;
                if (dic.TryGetValue(x, out y)) {
                    return y;
                } else {
                    var tmp = func(x);
                    dic.Add(x, tmp);
                    return tmp;
                }
            };
        }

        /// <summary>
        /// 2bit化、6 -> 110
        /// </summary>
        public static IEnumerable<bool> BitSeq(long x) {
            return BitSeqTmp(x).Reverse();
        }

        static IEnumerable<bool> BitSeqTmp(long x) {
            for (long i = x; i > 0L; i = i / 2L) {
                yield return Convert.ToBoolean(i % 2);
            }
        }

        /// <summary>
        /// べき乗
        /// </summary>
        public static long Power(long x, long y, long mod) {
            var bitSeq = Util.BitSeq(y);
            long tmp = x;
            long total = 1;
            foreach (var bit in bitSeq.Reverse()) {
                if (bit) {
                    total = total * tmp % mod;
                }
                tmp = tmp * tmp % mod;
            }
            return total;
        }

        /// <summary>
        /// Combination
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <param name="mod"></param>
        /// <param name="memRev"></param>
        /// <returns></returns>
        public static long Combination(long x, long y, long mod) {
            var memRev = Util.Memoize1<long, long>(Rev);
            var chunk = new List<long>();
            for (long i = x; i > x - y; i--) {
                chunk.Add(i);
            }
            var revChunk = new List<long>();
            for (long i = 1; i < y + 1; i++) {
                revChunk.Add(i);
            }
            return chunk
                .Concat(revChunk.Select(memRev))
                .Aggregate((acc, z) => acc * z % mod);

        }

        /// <summary>
        /// 逆元
        /// </summary>
        public static long Rev(long x) {
            var p = Convert.ToInt64(Math.Pow(10, 9)) + 7L;
            var bitArrayP = Util.BitSeq(p - 2L);
            long tmp = x;
            long total = 1;
            foreach (var bit in bitArrayP.Reverse()) {
                if (bit) {
                    total = total * tmp % p;
                }
                tmp = tmp * tmp % p;
            }
            return total;
        }
    }
}
