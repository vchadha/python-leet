class Solution:
  def lengthOfLongestSubstring(self, s: str) -> int:
    """
    Given a string s, find the length of the longest without duplicate characters.

    Use a sliding window approach over the string.
    As we move over the string, keep updating the max valid
    window we have seen so far.
    """

    seen: set[str] = set()

    left: int = 0
    max_length: int = 0

    for right, char in enumerate(s):
      # Keep shifting left bound till window is valid
      while char in seen:
        seen.remove(s[left])
        left += 1

      seen.add(char)

      # Check for new window max length
      max_length = max(max_length, right - left + 1)

    return max_length

# Test cases
# Create a single instance of the solution to run test cases
solution = Solution()

s1 = "abcabcbb"
test_1 = solution.lengthOfLongestSubstring(s1)
assert test_1 == 3, f"Expected {3} but got {test_1}"

s2 = "bbbbb"
test_2 = solution.lengthOfLongestSubstring(s2)
assert test_2 == 1, f"Expected {1} but got {test_2}"

s3 = "pwwkew"
test_3 = solution.lengthOfLongestSubstring(s3)
assert test_3 == 3, f"Expected {3} but got {test_3}"

s4 = "dvdf"
test_4 = solution.lengthOfLongestSubstring(s4)
assert test_4 == 3, f"Expected {3} but got {test_4}"
