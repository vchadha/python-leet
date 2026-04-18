class Solution:
    def gameOfLife(self, board: list[list[int]]) -> None:
        """
        Do not return anything, modify board in-place instead.
        """
        old_board: list[list[int]] = [row.copy() for row in board]

        # If alive (1)
            # If neighbors < 2 -> die

            # If neighbors = 2 || neighbors = 3 -> live

            # If neighbors > 3 -> die

        # If dead
            # If neighbors = 3 -> live

            # Else -> no-op
        
