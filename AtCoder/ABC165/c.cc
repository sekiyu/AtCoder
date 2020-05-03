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

vector<int> pattern(int n, int m, int i) {
  
}

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);

  int n, m, q;
  cin >> n >> m >> q;
  
  vector< vector<int> > xys(q);
  for (auto & xy : xys) {
    xy = vector<int>(4);
    for (auto & x : xy ) cin >> x;
  }
  
  for (auto & xy : xys) {
    for (auto & x : xy ) cout << x << " ";;
  }
 

}