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

  ll x;
  cin >> x;
  ll y = 100;
  ll counter = 0;
  while(y < x) {
    y *=1.01;
    ++counter;
  }
  cout << counter << endl;



}