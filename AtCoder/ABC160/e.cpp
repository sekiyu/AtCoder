#include <cmath>
#include <cstdio>
#include <vector>
#include <map>
#include <queue>
#include <list>
#include <stack>
#include <iostream>
#include <algorithm>
using namespace std;
#define rep(i, n) for (int i = 0; i < (n); ++i)
using ll = long long;
using P = pair<int, int>;

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);

  int x,y,a,b,c;
  cin >> x >> y >> a >> b >> c;
  priority_queue<int> ps;
  rep(i, a) {
    int x;
    cin >> x;
    ps.push(x);
  }
  priority_queue<int> qs;
  rep(i, b) {
    int x;
    cin >> x;
    qs.push(x);
  }
  priority_queue<int> rs;
  rep(i, c) {
    int x;
    cin >> x;
    rs.push(x);
  }

  ll total = 0;
  queue<int> pp;
  queue<int> qq;
  rep(i, x) {
    pp.push(ps.top());
    ps.pop();
  }
  rep(i, y) {
    qq.push(qs.top());
    qs.pop();
  }
  rep(i, x + y) {
    int am = pp.empty() ? 0 : pp.front();
    int bm = qq.empty() ? 0 : qq.front();
    int cm = rs.empty() ? 0 : rs.top();
    int m = max(am, max(bm, cm));
    total += m;
    if (am == m) {
      pp.pop();
    }
    else if (bm == m) {
      qq.pop();
    } else {
      rs.pop();
    }
  }
  cout << total << endl;
}
