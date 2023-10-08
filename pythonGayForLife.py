import os
import time
import numpy as np

def create_board(size):
    return np.random.choice([1, 0], size*size, p=[0.2, 0.8]).reshape(size, size)

def update_board(board):
    size = board.shape[0]
    new_board = board.copy()
    for i in range(size):
        for j in range(size):
            neighbors = [(i-1, j-1), (i-1, j), (i-1, j+1),
                        (i, j-1),               (i, j+1),
                        (i+1, j-1), (i+1, j), (i+1, j+1)]
            live_neighbors = 0
            for x, y in neighbors:
                if 0 <= x < size and 0 <= y < size and board[x, y] == 1:
                    live_neighbors += 1
            if board[i, j] == 1:
                if live_neighbors < 2 or live_neighbors > 3:
                    new_board[i, j] = 0
            else:
                if live_neighbors == 3:
                    new_board[i, j] = 1
    return new_board

def display_board(board):
    os.system('clear')  # Clear the terminal screen (for Unix-like systems)
    for row in board:
        print(' '.join(['#' if cell else '.' for cell in row]))
    print()

def animate_game_of_life(size, generations):
    board = create_board(size)
    for _ in range(generations):
        display_board(board)
        board = update_board(board)
        time.sleep(0.1)  # Adjust the delay to control the animation speed

if __name__ == "__main__":
    board_size = 30  # Change this to adjust the size of the grid
    num_generations = 100  # Change this to adjust the number of generations
    animate_game_of_life(board_size, num_generations)

