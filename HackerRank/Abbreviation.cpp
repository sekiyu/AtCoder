// #include <bits/stdc++.h>
// #include <bit>
#include <cctype>
#include <locale>
#include <iostream>
#include <queue>
using namespace std;
#define rep(i, n) for (int i = 0; i < (n); ++i)
std::locale l = std::locale::classic();

// Complete the abbreviation function below.
string abbreviation(string a, string b) {
  vector<bool> dp(b.size() + 1, false);
  dp[0] = true;
  for (const auto& ai : a) {
    vector<bool> dpp(dp);
    if (isupper(ai)) dpp[0] = false;
    rep(j, b.size()) {
      if (islower(ai) && islower(b[j])) {
        dpp[j + 1] = dp[j] || dp[j + 1] || dpp[j];
      } else if(islower(ai)) {
        if (toupper(ai, l) == b[j]) {
          dpp[j + 1] = dp[j] || dp[j + 1];
        } else { 
          dpp[j + 1] = dp[j + 1];
        }
      } else if(islower(b[j])) {
        if (ai == toupper(b[j], l)) {
          dpp[j + 1] = dp[j] || dpp[j];
        } else {
          dpp[j + 1] = dpp[j];
        }
      } else {
        dpp[j + 1] = ai == b[j] ? dp[j] : false;
      }
    }
    dp = dpp;
  }
  return dp[b.size()] ? "YES" : "NO";
}


int main()
{
    ofstream fout(getenv("OUTPUT_PATH"));

    int q;
    cin >> q;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    for (int q_itr = 0; q_itr < q; q_itr++) {
        string a;
        getline(cin, a);

        string b;
        getline(cin, b);

        string result = abbreviation(a, b);
        cout << result << "\n";
        fout << result << "\n";
    }

    fout.close();

    return 0;
}
