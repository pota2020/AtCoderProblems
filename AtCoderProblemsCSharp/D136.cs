using System;
using System.Collections.Generic;
using System.Text;

namespace AtCoderProblemsCSharp {
    class D136 {
        static void _D136(string[] args) {
            var n = Console.ReadLine() + "R";
            var charArray = n.ToCharArray();
            var chunkCount = 0;
            var repeatCount = 0;
            var chunkCount2 = 0;
            var dic = new Dictionary<int, int>();
            for (int i = 0; i < n.Length; i++) {
                if (i == 0) {
                    chunkCount++;
                    continue;
                }
                if (charArray[i] == charArray[i - 1]) {
                    chunkCount++;
                    continue;
                }
                repeatCount++;
                if (repeatCount % 2 == 1) {
                    chunkCount2 = chunkCount;
                    chunkCount = 1;
                    continue;
                }
                dic.Add(i - chunkCount - 1, CountChildren(chunkCount2, chunkCount));
                dic.Add(i - chunkCount, CountChildren(chunkCount, chunkCount2));
                chunkCount = 1;
            }
            var output = new StringBuilder();
            for (int i = 0; i < n.Length - 1; i++) {
                var v = 0;
                dic.TryGetValue(i, out v);
                output.Append(v.ToString() + " ");
            }
            Console.WriteLine(output);
        }

        private static int CountChildren(int a, int b) {
            if (a % 2 == 0 && b % 2 == 0)
                return (a + b) / 2;
            else if (a % 2 == 1 && b % 2 == 0)
                return (a + b + 1) / 2;
            else if (a % 2 == 0 && b % 2 == 1)
                return (a + b - 1) / 2;
            else
                return (a + b) / 2;
        }
    }
}
