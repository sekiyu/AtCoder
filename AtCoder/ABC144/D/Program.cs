using System;

// Water Bottle
namespace D
{
    class Program
    {
        public static double ToAngle(double radian)
        {
            return (double)(radian * 180 / Math.PI);
        }

        public static double Solve(double a, double b, double x)
        {
            var theta = a * a * b / 2 < x
                ? Math.Atan(2 / a * (b - x / (a * a)))
                : Math.Atan(a * b * b / (2 * x));
            return ToAngle(theta);            
        }

        static void Main(string[] args)
        {
            string[] line = Console.ReadLine().Split(' ');
            double a = Convert.ToDouble(line[0]);
            double b = Convert.ToDouble(line[1]);
            double x = Convert.ToDouble(line[2]);
            Console.WriteLine(Solve(a, b, x));
        }
    }
}
