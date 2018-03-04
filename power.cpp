// https://codelab.interviewbit.com/problems/power/
#include <iostream>

using namespace std;
namespace Solution {
  int power(string A);
}

void test(string A, int expect) {
  int result = Solution::power(A);
  if (result != expect) {
    cout << "Unexpected result for " << A << endl
         << "       got " << result << endl
         << "  expected " << expect << endl
         << endl;
  } else {
    cout << "Passed for " << A << endl;
  }
}

int main() {
  test("1", 0);
  test("2", 1);
  test("3", 0);
  test("4", 1);
  test("6", 0);
  test("1208925819614629174706176", 1);
  test("1208925819614629174706178", 0);
  test("120892581961462917470617812089258196146291747061781208925819614629174706178120892581961462917470617812089258196146291747061781208925819614629174706178", 0);
  return 0;
}


int Solution::power(string A) {
  int start = 0;
  int length = A.length();
  int divisor = 2;
  while (start < length) {
    bool starting = true;
    int num = 0;
    for (int i = start; i < length; i++) {
      num = num * 10 + A[i] - '0';
      A[i] = num / divisor + '0';
      num = num % divisor;
      if (starting && A[i] == '0') {
        start = i + 1;
      } else {
        starting = false;
      }
    }
    if (num != 0) return 0;
    if (start + 1 == length && A[start] == '1') return 1;
  }
  return 0;
}
