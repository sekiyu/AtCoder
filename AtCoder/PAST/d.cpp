#include <iostream>
#include <algorithm>
using namespace std;

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);

  int n;
  cin >> n;
  bool bs[n];
  for (auto &b : bs) b = false;
  int deprecated = -1;
  for (int i = 0; i < n; ++i) {
    int a;
    cin >> a;
    if (bs[a - 1]) {
      deprecated = a;
    } else {
      bs[a - 1] = true;
    }
  }

  // No trouble
  if (deprecated == -1) {
    cout << "Correct" << endl;
    return 0;
  }

  // Trouble occured
  cout << deprecated << " ";
  for (int i = 0; i < n; ++i) {
    if (!bs[i]) cout << i + 1 << endl;
  }
  return 0;
}