#include <iostream>
#include <vector>
#include <cmath>
#include <utility>
#include <queue>
#include <map>
#include <list>
#include <set>
#include <iomanip>
#define rep(i, n) for (int i = 0; i < (n); ++i)
using namespace std;
using ll = long long;


int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);
  int l;
  cin >> l;
  std::cout << std::fixed;
  cout << setprecision(6) <<pow((double)(l) / 3.0, 3 ) << endl;
  return 0;
}


