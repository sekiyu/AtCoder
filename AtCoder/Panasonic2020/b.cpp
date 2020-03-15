#include <vector>
#include <iostream>
#include <cmath>
using namespace std;

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);

  long long h, w;
  cin >> h >> w;
  if (h == 1 || w == 1) {
    cout << 1 << endl;
    return 0;
  }
  long long n = 0;
  n = ((h + 1) / 2) * ((w + 1) / 2) + (h / 2) * (w / 2);
  // for (int i = 0; i < h; ++i) {
  //   if (i % 2 == 0) n += (w + 1) / 2;
  //   else n += w / 2;
  // }
  cout << n << endl;

}