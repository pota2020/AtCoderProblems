using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;

namespace AtCoderProblemsCSharp {
    class D133 {
        static void _D133(string[] args) {
            var n = int.Parse(Console.ReadLine());
            var a = Console.ReadLine().Split().Select(long.Parse).ToArray();
            var totalD = a.Sum();

            var mlist = new long[n];
            mlist[0] = CalcM1(n, a, totalD);
            for (int i = 1; i < n; i++)
                mlist[i] = CalcM2(a[i - 1], mlist[i - 1]);

            var s = new StringBuilder();
            foreach (var item in mlist)
                s.Append(item).Append(" ");
            Console.WriteLine(s.ToString());
        }

        private static long CalcM1(int n, long[] a, long total) {
            var tmp = 0L;
            for (int i = 1; i < n; i += 2)
                tmp += a[i] * 2;
            return total - tmp;
        }

        private static long CalcM2(long d, long mBefore) {
            return d * 2 - mBefore;
        }
    }
}
