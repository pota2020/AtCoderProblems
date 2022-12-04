using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;

namespace AtCoderProblemsCSharp {
    class C139 {
        static int N { get; }
        static IReadOnlyList<int> H { get; }

        static C139() {
            N = Convert.ToInt32(Console.ReadLine());
            H = Console.ReadLine().Split().Select(f => Convert.ToInt32(f)).ToList();
        }

        static void _C139(string[] args) {
            Console.WriteLine(GetAnsMember().Max().ToString());
        }

        static IEnumerable<int> GetAnsMember() {
            var total = 0;
            foreach (var i in Enumerable.Range(0, N)) {
                if (i == N - 1 || H[i] < H[i + 1]) {
                    yield return total;
                    total = 0;
                    continue;
                }
                total++;
            }
        }
    }
}
