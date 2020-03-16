#include <iostream>

void solve(std::string s) {
  try
  {
    int n = std::stoi(s);
    std::cout << 2 * n << std::endl;
  }
  catch(const std::exception& e)
  {
    std::cout << "error" << std::endl;
  }  
}

void solve2(std::string s) {
  int num = 0;
  for (char & c : s) {
    if (!isdigit(c)) {
      std::cout << "error" << std::endl;
      return;
    }
    num = 10 * num + (c - '0');
  }
  std::cout << 2 * num << std::endl;
}

int main(){
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);
  
  std::string s;
  std::cin >> s;
  solve2(s);
  return 0;
}

