#include <iostream>
#include <vector>
#include <cmath>
#include <utility>
#include <queue>
#include <map>
#include <list>
#include <set>
#define rep(i, n) for (int i = 0; i < (n); ++i)
using namespace std;
using ll = long long;

int count(int n) {
  if (n < 2) return 0;
  return n * (n - 1) / 2;
}

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);
  int n, m;
  cin >> n >> m;
  cout << count(n) + count(m) << endl;
  return 0;
}


