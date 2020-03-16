#include <iostream>
using namespace::std;

void solve(int prev, int current) {
  if (prev == current) {
    cout << "stay" << endl;
  }
  if (prev < current) {
    cout << "up " << current - prev << endl;
  } 
  else {
    cout << "down " << prev - current << endl;
  }
}

int main(){
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);
  
  int n;
  cin >> n;
  int prev;
  cin >> prev;
  for (int i = 0; i < n - 1; ++i) {
    int current;
    cin >> current;
    solve(prev, current);
    prev = current;
  }
  return 0;
}

