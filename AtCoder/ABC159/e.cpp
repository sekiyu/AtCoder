#include <iostream>
#include <vector>
#include <cmath>
#include <utility>
#include <queue>
#include <map>
#include <list>
#include <set>
#include <numeric>
#define rep(i, n) for (int i = 0; i < (n); ++i)
using namespace std;
using ll = long long;

int h, w, k;
const static int MAX = 1000000;

vector<bool> pattern(int i) {
  vector<bool> ret;
  rep(j, h - 1) {
    ret.push_back(i % 2 == 0);
    i /= 2;
  }
  return ret;
}

int calc(int ip, const vector< vector<int> >& ss) {
  vector<bool> q(pattern(ip));
  vector<int> index(h, 0);
  for (int i = 1; i < h; ++i) index[i] = index[i - 1] + (q[i - 1] ? 1 : 0);
  // cout << "index : ";
  // for (auto ind : index) cout << ind << ",";

  int n = accumulate(q.begin(), q.end(), 0) + 1;// 分割数
  vector< vector<int> > counts(n, vector<int>(w, 0));
  rep(ih, h) {
    rep(iw, w) {
      counts[index[ih]][iw] += ss[ih][iw];
      if (counts[index[ih]][iw] > k) return MAX;
    }
  }
  // cout << endl << "counts : ";
  // for (auto v : counts) for (auto x : v) cout << x << ",";
  // cout << endl;

  int count = n - 1;
  vector<int> current(n, 0);
  rep(ih, n) current[ih] = counts[ih][0];
  rep(iw, w - 1) {
    rep(ih, n) {
      if (current[ih] + counts[ih][iw + 1] <= k) {
        current[ih] += counts[ih][iw + 1];
      }
      else {
        // cout << "counted " << ih << "," << iw << endl;
        ++count;
        rep(ih, n) current[ih] = counts[ih][iw + 1];
        break;
      }
    }    
  }
  return count;
}

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);
  cin >> h >> w >> k;
  vector< vector<int> > ss(h, vector<int>(w));
  rep(i, h) {
    string s;
    cin >> s;
    rep(j, w) ss[i][j] = s[j] - '0';
  }
  int ans = MAX;

  rep(i, pow(2, h - 1)) {    
    // for (auto b : pattern(i)) cout << b << ", ";
    // cout << "cost = ";
    // cout << calc(i, ss) << endl;
    ans = min(ans, calc(i, ss));
  }
  cout << ans << endl;
  return 0;
}


