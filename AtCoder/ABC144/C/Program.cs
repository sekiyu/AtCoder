using System;

namespace C
{
    class Program
    {
        private static long Solve(long n)
        {
            long either = (long)Math.Sqrt(n);            
            while (either > 0)
            {
                if (n % either == 0)
                {
                    var other = (long)(n / either);
                    return either + other - 2;
                }
                
                --either;
            }
            return n - 1;
        }

        static void Main(string[] args)
        {
            long n = long.Parse(Console.ReadLine());
            Console.WriteLine(Solve(n));
        }
    }
}
