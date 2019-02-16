using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOJ
{
    public class ALDS1_10_C
    {
        public static void Solve()
        {
            var N = int.Parse(Console.ReadLine());
            foreach (var i in Enumerable.Range(1, N))
            {
                var x = Console.ReadLine();
                var y = Console.ReadLine();
                Console.WriteLine(Lcs(x, y));
            }
        }

        static int Lcs(string x, string y)
        {
            var n = x.Length;
            var m = y.Length;
            var dp = new int[n+1, m+1];
            foreach (var i in Enumerable.Range(0,n+1))
            {
                foreach(var j in Enumerable.Range(0, m+1))
                {
                    if (i == 0 || j == 0)
                    {
                        dp[i, j] = 0;
                    }
                    else if (x[i-1] == y[j-1])
                    {
                        dp[i, j] = dp[i - 1, j - 1] + 1;
                    }
                    else
                    {
                        dp[i, j] = Math.Max(dp[i, j-1], dp[i-1, j]);
                    }
                }
            }
            return dp[n, m];
        }
    }
}
