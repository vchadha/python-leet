class Solution:
  def lengthOfLongestSubstring(self, s: str) -> int:
      

      in_progress: list[tuple[str, bool]] = []

      for char in s:
        for idx, substr_tup in enumerate(in_progress):
          if char in substr_tup[0] and substr_tup[1] == False:
            in_progress[idx] = (substr_tup[0], True)
          elif substr_tup[1] == False:
            in_progress[idx] = (substr_tup[0] + char, False)

        # Create new substring
        in_progress.append((char, False))

      # Find max substring length
      max_substring_length = 0

      for substr_tup in in_progress:
        str_length = len(substr_tup[0])
        if str_length > max_substring_length:
          max_substring_length = str_length

      return max_substring_length

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
