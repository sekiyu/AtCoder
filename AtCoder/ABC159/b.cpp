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

bool iskaibun(string s, int start, int end) {
  int n = s.size();
  for (int i = start; i <= end / 2; ++i) {
    if (s[i] != s[end - i - 1]) return false;
  }
  return true;
}

bool isStrongKaibun(string s) {
  int n = s.size();
  return iskaibun(s, 0, n) && iskaibun(s, 0, (n - 1) / 2) && iskaibun(s, (n + 3) / 2, n);
}

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);
  string s;
  cin >> s;

  cout << (isStrongKaibun(s) ? "Yes" : "No") << endl;
  return 0;
}


