# Definition for singly-linked list.
from typing import Optional

class ListNode:
    def __init__(self, val: int = 0, next: Optional[ListNode] = None):
        self.val = val
        self.next = next

class Solution:
    def addTwoNumbers(self, l1: Optional[ListNode], l2: Optional[ListNode]) -> Optional[ListNode]:
        """
        Adds two numbers represented by linked lists and returns the sum as a linked list.
        The digits are stored in reverse order, and each of their nodes contains a single digit.
        Values are between 0 and 9
        """
        dummy_head: Optional[ListNode] = ListNode()
        new_list_tail: Optional[ListNode] = dummy_head

        # Initialize remainder to 0
        remainder: int = 0

        # Loop until we have processed all nodes in both lists and there is no remainder left
        while l1 is not None or l2 is not None or remainder != 0:
            # Handle the case where one list is longer than the other
            val1 = l1.val if l1 is not None else 0
            val2 = l2.val if l2 is not None else 0

            # Sum the values and the remainder
            full_value = val1 + val2 + remainder

            # Get single digit and remainder for new node
            single_digit = full_value % 10
            remainder = full_value // 10

            # Create new node
            new_node = ListNode(single_digit)

            # Append new node to the new list
            new_list_tail.next = new_node
            new_list_tail = new_node

            # Update l1 and l2 to next nodes
            l1 = l1.next if l1 is not None else None
            l2 = l2.next if l2 is not None else None

        return dummy_head.next

# Test cases
# Create a single instance of the solution to run test cases
solution = Solution()

expected_output = [7,0,8]
output = solution.addTwoNumbers(ListNode(2, ListNode(4, ListNode(3))), ListNode(5, ListNode(6, ListNode(4))))
for i in range(len(expected_output)):
    assert output is not None, f"Expected {expected_output} but got None"
    assert output.val == expected_output[i], f"Expected {expected_output} but got {output.val} at index {i}"
    output = output.next
