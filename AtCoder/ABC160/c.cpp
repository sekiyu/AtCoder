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

  int k, n;
  cin >> k >> n;
  vector<int> as(n);
  for (auto & a : as ) cin >> a;
  vector<int> dists(n, 0);
  rep(i, n-1) {
    dists[i] = as[i+1] - as[i]; 
  }
  dists[n-1] = k + as[0] - as[n-1];
  int mn = *max_element(dists.begin(), dists.end());
  cout << k-mn << endl;

}