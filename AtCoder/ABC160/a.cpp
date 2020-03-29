#include <cmath>
#include <cstdio>
#include <vector>
#include <map>
#include <queue>
#include <list>
#include <iostream>
#include <algorithm>
using namespace std;
#define rep(i, n) for (int i = 0; i < (n); ++i)
using ll = long long;
using P = pair<int, int>;

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);

  string s;
  cin >> s;
  if (s[2] == s[3] && s[4] == s[5]) {
    cout << "Yes" << endl;
  } else {
    cout << "No" << endl;
  }

}