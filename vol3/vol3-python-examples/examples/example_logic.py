#!/usr/bin/env python
"""
    Artificial Intelligence for Humans
    Volume 3: Deep Learning and Neural Networks
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com
    Code repository:
    https://github.com/jeffheaton/aifh
    Copyright 2015 by Jeff Heaton
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
"""

class InputNeuron:
    def __init__(self):
        self.value = 0

    def compute(self):
        return self.value

class Neuron:
    def __init__(self, bias, parents):
        self.bias = bias
        self.parents = parents
        pass

    def compute(self):
        sum = self.bias
        for row in self.parents:
            sum+=row[0]*row[1].compute()

        return 1 if sum >= 0.5 else 0

def truth_table(inputs, neuron):
    v = [0] * len(inputs)
    done = False
    while not done:
        for i in range(len(inputs)):
            inputs[i].value = v[i]

        o = neuron.compute()
        print("{} : {}".format(v,o))

        # Roll forward to next row
        i = 0
        while i<len(v):
            v[i] += 1
            if v[i]>1:
                v[i] = 0
                i+=1
            else:
                break

        if i==len(v):
            done = True


def process_and():
    print("Boolean AND")
    inputs = [InputNeuron(),InputNeuron()]
    output = Neuron(-1.5,[[1,inputs[0]],[1,inputs[1]]])
    truth_table(inputs,output)

def process_or():
    print("Boolean OR")
    inputs = [InputNeuron(),InputNeuron()]
    output = Neuron(-0.5,[[1,inputs[0]],[1,inputs[1]]])
    truth_table(inputs,output)

def process_not():
    print("Boolean NOT")
    inputs = [InputNeuron()]
    output = Neuron(0.5,[[-1,inputs[0]]])
    truth_table(inputs,output)

def process_xor():
    print("Boolean XOR")
    inputs = [InputNeuron(),InputNeuron()]
    hidden1 = [
        Neuron(-0.5,[[1,inputs[0]],[1,inputs[1]]]),
        Neuron(-1.5,[[1,inputs[0]],[1,inputs[1]]])]

    hidden2 = Neuron(0.5,[[-1,hidden1[1]]])
    output = Neuron(-1.5,[[1,hidden1[0]],[1,hidden2]])
    truth_table(inputs,output)


process_and()
print()
process_or()
print()
process_not()
print()
process_xor()
