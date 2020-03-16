#include <iostream>
#include <algorithm>
using namespace std;


int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);

  int n = 6;
  int ns[n];
  for (auto &i : ns) {
    cin >> i;
  }
  sort(ns, ns + n);
  cout << ns[3] << endl;
  return 0;
}