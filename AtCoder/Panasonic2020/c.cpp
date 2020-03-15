#include <vector>
#include <iostream>
#include <cmath>
using namespace std;

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);

  long long a, b, c;
  cin >> a >> b >> c;

  if (c - a - b < 0) {
    cout << "No" << endl;
    return 0;
  } 
  if (4 * a * b < (c - a - b) * (c - a - b)) {
    cout << "Yes" << endl;
  } else {
    cout << "No" << endl;
  }
}