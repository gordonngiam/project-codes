import random
import math


#  The following conditions return false:
# 1. Player is trying to move a piece that is the opponent's
# 2. index is in INNER ring
# 3. push direction is same as  side of outer ring that index is on
def check_move(board, turn, index, push_from):
    n = int(math.sqrt(len(board)))  # note: len of board is guaranteed to be a squared number
    if board[index] != 0 and board[index] != turn:  # invalid move if player trying to move the opponent's piece
        return False

    # check if index at top row and push_from is top
    if 0 <= index < n and push_from == 'T':
        return False

    # check if index at bottom row and push_from is bottom
    if (n - 1) * n <= index < n * n and push_from == 'B':
        return False

    # check if index at left col and push_from is left or index at right col and push_from at right
    for i in range(0, n):
        left_col_index = i * n
        if left_col_index == index and push_from == 'L':
            return False

        right_col_index = left_col_index + n - 1
        if right_col_index == index and push_from == 'R':
            return False

    # check if index is in inner ring
    for i in range(1, n - 1):
        for j in range(1, n - 1):
            idx = i * n + j
            if idx == index:
                return False
    return True


# In this function, we assume all input are valid
# we then return aa new board that results from the current board and the move to be applied
def apply_move(board, turn, index, push_from):
    new_board = board.copy()
    n = int(math.sqrt(len(board)))  # note: len of board is guaranteed to be a squared number
    row = index // n  # row of index
    col = index % n  # col of index

    # we start from the index, and copy the value at the next corresponding index into it
    if push_from == 'T':  # push from top row
        curr_row = row
        curr_col = col
        while curr_row > 0:
            curr_index = curr_row * n + curr_col
            new_board[curr_index] = new_board[curr_index - n]
            curr_row -= 1
        new_board[col] = turn
    elif push_from == 'B':  # push from bottom row
        curr_row = row
        curr_col = col
        while curr_row < n - 1:
            curr_index = curr_row * n + curr_col
            new_board[curr_index] = new_board[curr_index + n]
            curr_row += 1
        new_board[(n - 1) * n + col] = turn
    elif push_from == 'L':  # push from left col
        curr_row = row
        curr_col = col
        while curr_col > 0:
            curr_index = curr_row * n + curr_col
            new_board[curr_index] = new_board[curr_index - 1]
            curr_col -= 1
        new_board[row * n] = turn
    elif push_from == 'R':  # push from right col
        curr_row = row
        curr_col = col
        while curr_col < n - 1:
            curr_index = curr_row * n + curr_col
            new_board[curr_index] = new_board[curr_index + 1]
            curr_col += 1
        new_board[row * n + (n - 1)] = turn
    return new_board


# Returns true if we can find a victorious player
def check_victory(board, who_played):
    n = int(math.sqrt(len(board)))  # note: len of board is guaranteed to be a squared number

    player1_won = False
    player2_won = False

    # check horizontal
    for i in range(n):
        player = board[i * n]
        if player == 0:
            continue
        have_winner = True
        for j in range(n):
            index = i * n + j
            if player != board[index]:
                have_winner = False
                break
        if have_winner:
            if player == 1:
                player1_won = True
            if player == 2:
                player2_won = True

    # check vertical
    for col in range(n):
        player = board[col]
        if player == 0:
            continue
        have_winner = True
        for row in range(n):
            index = row * n + col
            if player != board[index]:
                have_winner = False
                break
        if have_winner:
            if player == 1:
                player1_won = True
            if player == 2:
                player2_won = True

    # check bottom left to top right diagonal (/)
    player = board[(n - 1) * n]
    if player != 0:
        have_winner = True
        for row in reversed(range(n)):
            col = (n - 1) - row
            index = row * n + col
            if player != board[index]:
                have_winner = False
                break
        if have_winner:
            if player == 1:
                player1_won = True
            if player == 2:
                player2_won = True

    # check top left to bottom right diagonal (\)
    player = board[0]
    if player != 0:
        have_winner = True
        for row in range(n):
            col = row
            index = row * n + col
            if player != board[index]:
                have_winner = False
                break
        if have_winner:
            if player == 1:
                player1_won = True
            if player == 2:
                player2_won = True

    if player1_won and player2_won:
        if who_played == 1:
            return 2
        else:
            return 1
    elif player1_won:
        return 1
    elif player2_won:
        return 2
    else:
        return 0


def computer_move(board, turn, level):
    # generate top, bottom, left, indexes based on n
    def generate_valid_indexes(n):
        # top and bottom rows
        idx_set = set()
        for i in range(n):
            idx_set.add(i)  # top row
            idx_set.add((n - 1) * n + i)  # bototm row
            idx_set.add(i * n)  # left col
            idx_set.add(i * n + (n - 1))  # right col
        return idx_set

    # get the opponent turn based on the given turn
    def get_opp_turn(turn):
        if turn == 1:
            return 2
        else:
            return 1

    n = int(math.sqrt(len(board)))  # note: len of board is guaranteed to be a squared number
    directions = ("T", "B", "L", "R")
    indexes = generate_valid_indexes(n)
    if level == 1:  # generate random move
        while True:
            rand_dir = random.randint(0, 3)
            index = list(indexes)[random.randint(0, len(indexes) - 1)]
            if check_move(board, turn, index, directions[rand_dir]):
                return index, directions[rand_dir]
    elif level == 2:
        new_boards = []
        moves = []
        # check if there exists a next move that causes computer to win
        for direction in directions:
            for index in indexes:
                if check_move(board, turn, index, direction):  # check if this move is valid
                    new_board = apply_move(board, turn, index, direction)  # get new board
                    new_boards.append(new_board)
                    moves.append((index, direction))
                    if check_victory(new_board, turn) == turn:  # if the bot can win with this move, return it
                        return index, direction

        # if computer cannot win , check if any  of our prev moves will cause direct win for opponent.
        # return any safe move.
        opp_turn = get_opp_turn(turn)
        safe = None
        opp_can_win_next_round = False
        for i in range(len(new_boards)):
            board = new_boards[i]
            is_safe = True
            for opp_direction in directions:
                for opp_index in indexes:
                    if check_move(board, opp_turn, opp_index, opp_direction):  # check if this move is valid
                        if check_victory(apply_move(board, opp_turn, opp_index, opp_direction),
                                         opp_turn) == opp_turn:
                            opp_can_win_next_round = True
                            is_safe = False
            if is_safe:
                safe = moves[i]

        if not opp_can_win_next_round:
            return computer_move(board, turn, 1)
        elif opp_can_win_next_round:
            if not safe:
                return computer_move(board, turn, 1)
            else:
                return safe


def display_board(board):
    n = int(math.sqrt(len(board)))  # note: len of board is guaranteed to be a squared number
    for i in range(n):
        for j in range(n):
            index = i * n + j
            to_print = ""
            value = board[index]
            if value == 0:
                to_print = ' '
            elif value == 1:
                to_print = 'X'
            elif value == 2:
                to_print = 'O'

            print("|" + to_print, end='')  # setting end='' omits the newline character
        print('|')


def menu():
    # get size of board
    n = 5
    while True:  # loop to validate user input for board size
        user_input = input("Enter board size (leave blank for default value of 5): ").strip()
        if not user_input:  # if user leaves blank, set to default value 5
            break
        if user_input.isdigit() and int(user_input) >= 3:  # validate user input
            n = int(user_input)
            break
        else:
            print("Invalid board size. Please try again!")

    # get type of player
    computer_level = 0  # 0 means human
    while True:  # loop to validate user input for human or computer
        print("Choose an opponent. Human or Computer?")
        user_input = input("Enter 'H' for Human opponent, or 'C' for Computer opponent: ").strip()
        if user_input == 'H':
            break
        elif user_input == 'C':
            while True:  # loop to validate computer level
                level = input("Enter computer level (1 or 2): ")
                if level.isdigit() and 1 <= int(level) <= 2:
                    computer_level = int(level)
                    break
                else:
                    print("Invalid computer level. Please try again!")
            break
        else:
            print("Invalid input. Please try again!")

    # initialize game values
    board = [0] * (n ** 2)
    turn = 1

    print("Lets start playing QUIXO!")
    print()
    while True:  # Overall game loop. Breaks when game is over.
        print("--------------------------------------")
        print("Player " + str(turn) + "'s turn")
        display_board(board)
        index = -1
        direction = ''
        if computer_level == 0 or turn == 1:  # if its a non-computer player's turn
            while True:  # loop to validate index
                index = input("Enter index: ").strip()
                if index.isdigit() and 0 <= int(index) < len(board):
                    index = int(index)
                    break
                else:
                    print("Invalid index. Please try again!")

            while True:  # loop to validate push direction
                direction = input('Enter push direction (T/B/L/R): ').strip()
                if direction in ('T', 'B', 'L', 'R'):
                    break
                else:
                    print("Invalid push direction. Please try again!")
        else:  # if it is a computer player, we get the index and direction
            index, direction = computer_move(board, turn, computer_level)
        if check_move(board, turn, index, direction):  # check if move is valid
            board = apply_move(board, turn, index, direction)  # apply move
            print("(" + str(index) + ", " + str(direction) + ") move played!")
            victory = check_victory(board, turn)  # check if anyone is victorious
            if victory == 0:  # no winning situation, set turn to next player
                if turn == 1:
                    turn = 2
                else:
                    turn = 1
                continue
            elif victory == 1:  # player 1 wins, break from loop.
                print("Player 1 won!")
                display_board(board)
                break
            elif victory == 2:  # player 2 wins, break from loop.
                print("Player 2 won!")
                display_board(board)
                break
        else:  # if invalid move
            print("(" + str(index) + ", " + str(direction) + ") is an invalid move. Please try again!")
            continue
    print("-------------------------- end of game --------------------------")


if __name__ == "__main__":
    menu()
