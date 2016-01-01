#!/usr/bin/env python
"""
    Artificial Intelligence for Humans
    Volume 1: Fundamental Algorithms
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com

    Code repository:
    https://github.com/jeffheaton/aifh

    Copyright 2013 by Jeff Heaton

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

    For more information on Heaton Research copyrights, licenses
    and trademarks visit:
    http://www.heatonresearch.com/copyright
============================================================================================================
This example shows how to do very basic OCR using distance metrics.  To use this program draw
a character under "Draw Here".  Then input the letter that you drew in the box next to "Learn:".
Click the "Learn:" button and this character is added to the trained characters.  Repeat this for
a few characters.  Finally, draw a character and click recognize.  Your previous characters will
be scanned and the character with the shortest distance is shown.
"""
__author__ = 'jheaton'

try:
    # for Python2
    import Tkinter as tk
    import tkMessageBox
except ImportError:
    # for Python3
    import tkinter as tk
    import tkinter.messagebox


import sys
from scipy.spatial import distance


class Application(tk.Frame):
    DRAW_AREA = 256
    DOWN_SAMPLE_WIDTH = 5
    DOWN_SAMPLE_HEIGHT = 7

    def __init__(self, master=None):
        tk.Frame.__init__(self, master)
        self.grid()
        self.b1 = None
        self.canvas_draw = None
        self.x_old = None
        self.y_old = None
        self.button_quit = None
        self.button_recognize = None
        self.button_learn = None
        self.entry_learn_char = None
        self.button_clear = None
        self.list_learned = None

        self.learned_patterns = {}

        self.create_widgets()
        self.clear()


    def create_widgets(self):
        l1 = tk.Label(self, text="Draw Here")
        l1.grid(row=0, column=0)
        l1 = tk.Label(self, text="Trained Characters")
        l1.grid(row=0, column=1, columnspan=2)
        self.canvas_draw = tk.Canvas(self, width=Application.DRAW_AREA, height=Application.DRAW_AREA)
        self.canvas_draw.grid(row=1, column=0)
        self.list_learned = tk.Listbox(self, height=10, )
        self.list_learned.grid(row=1, column=1, sticky=tk.N + tk.E + tk.S + tk.W, columnspan=2)

        self.button_learn = tk.Button(self, text='Learn:', command=self.learn)
        self.button_learn.grid(row=2, column=0, sticky=tk.N + tk.E + tk.S + tk.W)
        self.entry_learn_char = tk.Entry(self)
        self.entry_learn_char.grid(row=2, column=1, sticky=tk.N + tk.E + tk.S + tk.W, columnspan=2)

        self.button_recognize = tk.Button(self, text='Recognize', command=self.recognize)
        self.button_recognize.grid(row=3, column=0, sticky=tk.N + tk.E + tk.S + tk.W)
        self.button_quit = tk.Button(self, text='Quit', command=self.quit)
        self.button_quit.grid(row=3, column=1, sticky=tk.N + tk.E + tk.S + tk.W)
        self.button_clear = tk.Button(self, text='Clear', command=self.clear)
        self.button_clear.grid(row=3, column=2, sticky=tk.N + tk.E + tk.S + tk.W)

        self.canvas_draw.bind("<Motion>", self.motion)
        self.canvas_draw.bind("<ButtonPress-1>", self.b1down)
        self.canvas_draw.bind("<ButtonRelease-1>", self.b1up)

    def b1down(self, event):
        self.b1 = "down"           # you only want to draw when the button is down
        # because "Motion" events happen -all the time-

    def b1up(self, event):
        self.b1 = "up"
        self.x_old = None           # reset the line when you let go of the button
        self.y_old = None

    def motion(self, event):
        if self.b1 == "down":
            if self.x_old is not None and self.y_old is not None:
                event.widget.create_line(self.x_old, self.y_old, event.x, event.y, smooth=tk.TRUE)
                #self.plot_line(self.xold,self.yold,event.x,event.y)
                self.draw_data[event.y][event.x] = True
            self.x_old = event.x
            self.y_old = event.y

    def vertical_line_clear(self, col):
        for row in range(0, Application.DRAW_AREA):
            if self.draw_data[row][col]:
                return False
        return True

    def horizontal_line_clear(self, row):
        for col in range(0, Application.DRAW_AREA):
            if self.draw_data[row][col]:
                return False
        return True

    def down_sample_region(self, x, y):
        start_x = int(self.clip_left + (x * self.ratioX))
        start_x = int(self.clip_top + (y * self.ratioY))
        end_x = int(start_x + self.ratioX)
        end_y = int(start_x + self.ratioY)

        for yy in range(start_x, end_y + 1):
            for xx in range(start_x, end_x + 1):
                if self.draw_data[yy][xx]:
                    return True

        return False

    def down_sample(self):
        # Find bounding rectangle.
        # Find left side of bounding rectangle
        self.clip_left = 0
        for col in range(0, Application.DRAW_AREA):
            if not self.vertical_line_clear(col):
                self.clip_left = col
                break
                # Find right side of bounding rectangle
        self.clip_right = 0
        for col in range(Application.DRAW_AREA - 1, -1, -1):
            if not self.vertical_line_clear(col):
                self.clip_right = col
                break
                # Find top side of bounding rectangle
        self.clip_top = 0
        for row in range(0, Application.DRAW_AREA):
            if not self.horizontal_line_clear(row):
                self.clip_top = row
                break
                # Find bottom side of bounding rectangle
        self.clip_bottom = 0
        for row in range(Application.DRAW_AREA - 1, -1, -1):
            if not self.horizontal_line_clear(row):
                self.clip_bottom = row
                break

        self.canvas_draw.create_rectangle(
            self.clip_left,
            self.clip_top,
            self.clip_right,
            self.clip_bottom)

        # Now down sample to 5x7.
        result = []

        self.ratioX = float(self.clip_right - self.clip_left) / Application.DOWN_SAMPLE_WIDTH
        self.ratioY = float(self.clip_bottom - self.clip_top) / Application.DOWN_SAMPLE_HEIGHT

        for y in range(0, Application.DOWN_SAMPLE_HEIGHT):
            for x in range(0, Application.DOWN_SAMPLE_WIDTH):
                if self.down_sample_region(x, y):
                    result.append(1)
                else:
                    result.append(0)

        return result

    def clear(self):
        self.entry_learn_char.delete(0, tk.END)
        self.canvas_draw.delete("all")
        self.draw_data = [[False] * Application.DRAW_AREA for _ in range(Application.DRAW_AREA)]

    def recognize(self):
        best = "?"
        best_distance = sys.float_info.max

        sample = self.down_sample()

        for key in self.learned_patterns.keys():
            other_sample = self.learned_patterns[key]
            dist = distance.euclidean(sample, other_sample)
            if dist < best_distance:
                best_distance = dist
                best = key
        tkMessageBox.showinfo("Learn", "I believe you drew a: " + best)


    def learn(self):
        learned_char = self.entry_learn_char.get()

        if len(learned_char) > 1 or len(learned_char) == 0:
            tkMessageBox.showinfo("Learn", "Please enter a single character to learn")
            return

        if learned_char in self.learned_patterns:
            tkMessageBox.showinfo("Learn", "Already learned that character, please choose another")
            return

        self.list_learned.insert(tk.END, learned_char)
        self.learned_patterns[learned_char] = self.down_sample()

        # Clear and notify user.
        self.clear()
        tkMessageBox.showinfo("Learn", "Learned the pattern for: " + learned_char)


app = Application()
app.master.title('Python OCR')
app.mainloop()