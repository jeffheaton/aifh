__author__ = 'jheaton'

# for python 3.x use 'tkinter' rather than 'Tkinter'
from Tkinter import *
import time
import random

CANVAS_WIDTH = 400
CANVAS_HEIGHT = 400
CELL_WIDTH = 3
CELL_HEIGHT = 3


class App():
    """
    Elementary Cellular Automation (ECA)


    """

    RULE = 30

    def __init__(self):
        self.root = Tk()
        l1=Label(self.root,text="Rule:" + str(App.RULE))
        l1.pack()
        self.c = Canvas(self.root,width=400, height=400)
        self.c.pack()

        rows = CANVAS_HEIGHT / CELL_HEIGHT
        cols = CANVAS_WIDTH / CELL_WIDTH

        grid = [[0 for x in range(rows)] for x in range(cols)]

        # Seed the grid
        grid[0][cols/2] = True

        # Decode the rule
        output = [0] * 8

        cx = 1
        idx = 7
        while idx > 0:
            output[idx] = (App.RULE & cx) != 0
            idx = idx - 1
            cx *= 2

        # Build the CA.
        for row in range(0,rows):
            prev_row = row - 1


            for i in range(0, cols - 2):
                x = i * CELL_WIDTH
                y = row * CELL_HEIGHT

                if prev_row>=0:
                    result = False
                    a = grid[prev_row][i]
                    b = grid[prev_row][i + 1]
                    c = grid[prev_row][i + 2]

                    if a and b and c:
                        result = output[0]
                    elif a and b and not c:
                        result = output[1]
                    elif a and not b and c:
                        result = output[2]
                    elif a and not b and not c:
                        result = output[3]
                    elif not a and b and c:
                        result = output[4]
                    elif not a and b and not c:
                        result = output[5]
                    elif not a and not b and c:
                        result = output[6]
                    elif not a and not b and not c:
                        result = output[7]

                    grid[row][i + 1] = result

                if grid[row][i + 1] :
                    r = self.c.create_rectangle(x, y, x+CELL_WIDTH,y+CELL_HEIGHT, outline="black", fill="black")
                else:
                    r = self.c.create_rectangle(x, y, x+CELL_WIDTH,y+CELL_HEIGHT, outline="black", fill="white")


        self.root.mainloop()


app=App()