
def lcs(xs, ys):
  n = len(xs)
  m = len(ys)
  dp = [[0 for j in range(m+1)] for i in range(n+1)]
  for i, x in enumerate(xs):
    for j, y in enumerate(ys):
      if x == y:
        dp[i+1][j+1] = 1 + dp[i][j]
      else:
        dp[i+1][j+1] = max(dp[i][j+1], dp[i+1][j])
  return dp[n][m]

def solve():
    N = int(input())
    for _ in range(N):
        x = input()
        y = input()
        print(lcs(x, y))
 
if __name__ == '__main__':
    solve()