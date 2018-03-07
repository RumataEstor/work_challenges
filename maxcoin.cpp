// https://codelab.interviewbit.com/problems/maxcoin/
#include <iostream>
#include <sstream>
#include <vector>
#include <map>

using namespace std;
namespace Solution {
  int maxcoin(vector<int> &A);
}

vector<int> s_to_v(string s) {
  vector<int> v;
  stringstream r(s);
  int i;
  while (r >> i) {
    v.push_back(i);
    if (r.peek() == ' ') r.ignore();
  }
  return v;
}

int test(string input, int expect) {
  static int test_num = 0;
  test_num++;

  auto A = s_to_v(input);
  auto result = Solution::maxcoin(A);
  if (result == expect) {
    cout << "Passed test #" << test_num << endl;
    return 0;
  }

  cout << "Failed test #" << test_num << endl
       << "       got " << result << endl
       << "  expected " << expect << endl
       << endl;
  return 1;
}

int main() {
  return 0
    + test("1 2", 2)
    + test("1 2 3 4", 6)
    + test("28 14 34 18 20 34 17 24 7 22 17 49 49 20 46 22 18 46 45 39 50"
           " 40 17 48 23 0 0 0 18 46 45 39 50 40 17 48 23", 524)
    + 0;
}

// start for codelab

typedef pair<int, int> indexKey;
typedef pair<int, int> playersCoins;
typedef map<indexKey, playersCoins> memo_map;

playersCoins findBest(const vector<int> &A, int start, int end, memo_map &memo) {
    if (start == end) {
        return make_pair(A[start], 0);
    }

    auto key = make_pair(start, end);
    auto value = memo.find(key);
    if (value != memo.end()) {
        return value->second;
    }

    auto startBest = findBest(A, start + 1, end, memo);
    auto startValue = A[start] - startBest.first + startBest.second;
    auto endBest = findBest(A, start, end - 1, memo);
    auto endValue = A[end] - endBest.first + endBest.second;

    playersCoins best;
    if (startValue >= endValue) {
        best = make_pair(startBest.second + A[start], startBest.first);
    } else {
        best = make_pair(endBest.second + A[end], endBest.first);
    }

    memo[key] = best;
    return best;
}

int Solution::maxcoin(vector<int> &A) {
    memo_map memo;
    auto result = findBest(A, 0, A.size() - 1, memo);
    return result.first;
}
