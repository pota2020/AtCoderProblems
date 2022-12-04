using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace AtCoderProblemsCSharp {
    class C180 {
        static void _C180(string[] args) {
            Console.SetOut(new StreamWriter(Console.OpenStandardOutput()) { AutoFlush = false });
            var n = Convert.ToInt64(Console.ReadLine());
            var divList = new List<long>();
            var max = (long)Math.Sqrt(n);
            for (var i = 1L; i < max + 2L; i++) {
                if (n % i == 0) {
                    divList.Add(i);
                    divList.Add(n / i);
                }
            }
            foreach (var item in divList.OrderBy(x => x).Distinct()) {
                Console.WriteLine(item.ToString());
            }
            Console.Out.Flush();
        }
    }
}
