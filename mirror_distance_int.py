class Solution:
    def mirrorDistance(self, n: int) -> int:
        """
        Calculate the mirror distance for a given integer n.
        
        The mirror distance is defined as abs(n - reverse(n))
        where reverse(n) is the integer obtained by reversing the digits of n.
        """
        return abs(n - self.reverse(n))

    def reverse(self, n: int) -> int:
        """
        Reverse the digits of an integer n.
        """
        reversed_str = str(n)[::-1]
        return int(reversed_str)

# Test cases
# Create a single instance of the solution to run test cases
solution = Solution()
test_1 = solution.mirrorDistance(25)
assert test_1 == 27, f"Expected 27 but got {test_1}"

test_2 = solution.mirrorDistance(10)
assert test_2 == 9, f"Expected 9 but got {test_2}"

test_3 = solution.mirrorDistance(7)
assert test_3 == 0, f"Expected 0 but got {test_3}"
