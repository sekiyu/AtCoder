using System;
using System.Linq;
using System.Collections.Generic;
// Gluttony
namespace E
{
    class Program
    {
        static long Solve(int n, long k, List<long> a, List<long> f)
        {
            a.Sort();
            f.Sort();
            f.Reverse();
            var scores = a.Zip(f, (ai, fi) => new { ai, fi });
            var max = scores.Select(s => s.ai * s.fi).Max();

            Func<long, bool> pred = x =>
            {
                return scores.Select(score => Math.Max(0, score.ai - x / score.fi)).Sum() <= k;
            };

            Func<long, long, long> binarySearch = null;
            binarySearch = (start, end) => {
                if (start + 1 >= end)
                {
                    return pred(start)
                    ? start
                    : end;
                }
                var m = (start + end) / 2;
                return pred(m)
                    ? binarySearch(start, m)
                    : binarySearch(m, end);
            };

            return binarySearch(0, max);
        }


        static void Main(string[] args)
        {
            string[] line = Console.ReadLine().Split(' ');
            int n = Convert.ToInt32(line[0]);
            long k = Convert.ToInt64(line[1]);
            var a = Console.ReadLine().Split(' ').Select(x => Convert.ToInt64(x));
            var f = Console.ReadLine().Split(' ').Select(x => Convert.ToInt64(x));
            Console.Write(Solve(n, k, a.ToList(), f.ToList()));

        }
    }
}
