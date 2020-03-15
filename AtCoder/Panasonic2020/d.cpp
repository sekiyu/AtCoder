#include <vector>
#include <iostream>
#include <cmath>
#include <cstdio>
#define rep(i, n) for (int i = 0; i < (n); ++i)
using namespace std;
using ll = long long;

// char g[10];
// void gDfs(int i, int n, int mxChar) {
//   if (i == n) {
//     // cout << g << endl;
//     printf("%s\n", g);
//     return;
//   }
//   rep(j, mxChar + 1) {
//     g[i] = (char)('a' + j);
//     gDfs(i + 1, n, j == mxChar ? (mxChar + 1) : mxChar);
//   }
// }

void dfs(int i, int n, int mxChar, const string& s) {
  if (i == n) {
    // cout << s << endl;
    printf("%s\n", s.c_str());
    return;
  }
  rep(j, mxChar + 1) {    
    string ss(s);
    ss.push_back((char)('a' + j));
    dfs(i + 1, n, j == mxChar ? (mxChar + 1) : mxChar, ss);
  }
}

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);

  int n;
  cin >> n;
  dfs(0, n, 0, "");
  // gDfs(0, n, 0);
}