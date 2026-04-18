class Solution:
    BASIS: int = 10

    def mirrorDistance(self, n: int) -> int:
        """
        Calculate the mirror distance for a given integer n.
        
        The mirror distance is defined as abs(n - reverse(n))
        where reverse(n) is the integer obtained by reversing the digits of n.
        """
        # Reject input where n is negative
        if n < 0:
            raise ValueError("Input must be a non-negative integer")

        return abs(n - self.reverse(n))

    def reverse(self, n: int) -> int:
        """
        Reverse the digits of an integer n.
        """
        reversed_int = 0

        while n > 0:
            # Get last digit
            digit = n % 10

            # Append to end of reversed int
            reversed_int = reversed_int * Solution.BASIS + digit

            # Remove last digit
            n //= 10

        return reversed_int

# Test cases
# Create a single instance of the solution to run test cases
solution = Solution()
test_1 = solution.mirrorDistance(25)
assert test_1 == 27, f"Expected 27 but got {test_1}"

test_2 = solution.mirrorDistance(10)
assert test_2 == 9, f"Expected 9 but got {test_2}"

test_3 = solution.mirrorDistance(7)
assert test_3 == 0, f"Expected 0 but got {test_3}"
