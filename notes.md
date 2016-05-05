# Artificial Neural Networks in AKKA

### Premise

This project is an attempt to use an artificial neural network as a data flow transformer. Having an input stream of symbols which can be decoded into an input vector, the network will be able to generate a stream of more abstract symbols, using as additional information both the context (ie. data which was received before) and time gaps between consecutive chunks of data. The network is reactive - its computations are triggered by the input stream itself, not by a third agent watching the stream and sending requests to the network - and it can simultaneously receive and generate data.

### Introduction

In reasearch on artificial neural networks we can roughly distinguish two popular approaches:
1. Mathematical modelling. This is the approach that gave us Multi-Layered Perceptron and the Hopfield network. It describes artificial neural networks as mathematical models, where connections between neurons can be simplified to numbers in a matrix, the input signals as numbers in a vector, and in order to simulate the network's activity, instead of actually building the network it is enough to multiply the vector by the matrix. As you can easily guess, the learning algorithms of such networks are also just mathematical equations which we use to modify the matrix. Such networks can be used then to solve optimization problems, like the Travelling Salesman Problem, the Knapsack Problem and others. They all have their traditional solutions and using artificial neural networks in order to achieve the same is usually just a way to show to the students that artificial neural networks can be useful.
2. Spiking neural networks, or more generally simulations of a working brain. With more and more powerful computers it has become possible to simulate in detail how an organic brain works. Tightly connected to neurobiology, this approach takes what we know about how neurons are built and how they react to neurotransmitters and model the activity of a nervous system consisting of thousands, or even millions of such simulated neurons, each described by many variables. This way we can get a valuable insight into how we think and - in consequence - who we are.  

The approach I use in this work lies in the middle. I draw inspiration from how organic neurons work, but I focus on a few of their most important traits and simplify the ways they cooperate - not so much that it would be possible to describe ith a single mathematical equation, but enough to track and aticipate their activity. Each neuron receives signals from other neurons, process them taking into consideration only a few variables (ie. its own internal state) and send the result to yet other neurons. This is similar to the mathematical modelling, but on the other hand, in resemblance to an organic brain, each of the neurons is a separate entity - an "actor" - and it acts independent from others. As a matter of fact, such a neuron does not even has to know that it is a part of a network, just as our neurons do not know they are parts of our brains - they just react to the stimuli wherever it comes from.

My goal is to build an artificial neural network which works in a real time: signals would arrive to neuron in certain time intervals, neurons process them and the context - the fact that a moment ago another signal was processed and that the time gap between the consecutive signals is such and such - is also used as information, useful for processing subsequent signals.

To use a more poetic metaphor: Think about the development of aviation. In the beginning people were trying to imitate movements of birds. Such research helped us understand how birds fly and let us write down rules of aerodynamics, but airplanes do not fly the way birds do. The flight of an airplane is based on the same rules, but instead of a complex system of muscles, joints and remiges (flight feathers), we introduced a "cleaner" - from the engineering point of view - system which separates rigid wings from a propulsion engine.


### A neuron

A neuron in my model is implemented as an AKKA actor. It is asynchronous, meaning that there is no list of neurons which goes through a loop and each neuron is triggered one after another - they all are independent on each other and do not have to know they are part of a bigger network. It communicates with the outside world through messages. It can receive and process only one message at the time and it is reactive, ie. it works only when it receives messages - there are no tasks processed in the time gaps between them. For the simplicity, from now on I will call it an asynchronous neuron.
I distinguish three types of asynchronous neurons:
1. A dummy input neuron: Used as an entry point for signals coming in from the outside of the network, its only purpose is to pass the signal to other neurons.
2. A standard working neuron: Consists of constants set at the creation of the neuron and variables which change during the time of work. They are all discussed in detail below.
3. A hushing neuron: A special type used to send to other neurons "hush" messages in order to silence them.

There are no distinction between middle and output layers of the network. Every standard working neuron can be marked as an output one and then, when it fires, the program which encapsulates the network pushes a given symbol into the output stream.

---
Assuming it works, such a neural network is a reactive stream transformer: It takes a stream of data, perform operations and push out another stream of data. To the outside world, the network presents a very simple API: at the input point, we can put a vector of float numbers in the range of <0..1>, and at the output point we can retrieve another such vector. The received input vector is distributed among input neurons, one neuron for one element of the vector. The input neurons pass them as signals to other neurons which eventually send their own signals to yet other neurons, and after some time these signals reach output neurons and put together into the output vector. This is how all neural networks do it, but in case of ANNA there are three important differences.

1. If you push another input vector in ANNA, then - if the network still computes the output for the first vector - the result of those computations may be affected. Also, the results of computations of the first input vector may affect the output of the second one.
2. The time gap between consecutive input vectors is also affecting the computations.
3. The network may accept new input vectors while still working on previous ones.

In short, the network is not an INPUT => OUTPUT transformer, but INPUT + Internal State => OUTPUT, where Internal State is the sum of internal states of all its neurons, which are affected by previous computations and time gaps between them. 

How are time gaps useful? Why not hit the network with all data at once? Well, you could do that, but I would argue that in such a case you don't really want to use an asynchronous neural network. An asynchronous neural network is naturally prepared to work within the time context. And in fact this makes it similar to how our brains work. Imagine that when starting to listen to a song you were immediately provided with all sounds at once. It would make no sense. What changes a bunch of sounds into a song are their length, their sequence and the time gaps between them.

# Proof of concept

The concept is this: We can use a network of reactive asynchronous actors working as neurons in order to transform data chunks incoming in a form of a stream into a stream of data chunks of another type. Such a network can be relatively small and simple when compared to other types of artificial neural networks able to perform such tasks and it demostrates some interesting features absent from those networks, such as usage of time gaps between consecutive data chunks as additional information. 

The following example proves this concept. We will start with an input stream providing the Morse's code and we will produce an output stream of symbols showing the S.O.S. message.

The data chunks in the input stream will actually consist of only one symbol. Let's call it '1'. If in a given interval of time the network will receive one such symbol and then there will come nothing new for at least another such interval, the network should recognize it to be a 'dot' in the Morse's code. Internally, the recognition means that the neuron, which the network's designer tagged as the 'dot' neuron, fires. If the '1' symbol is immediately followed by another, the network should recognize it to be a 'line' (ie. another neuron, tagged as the 'line' neuron, fires). The network should clearly distinguish the two: It should not answer that what it received is both a 'dot' and a 'line', nor should it answer first that the received input is a 'dot' and then, without receiving any new data, change its mind and say it is a 'line' (or the other way around).

Being able to recognize dots and lines, the network should store this information in its internal state until it receives enough data to proceed further: Three 'dots' one after another should result in the 'S' symbol being produced. Three 'lines' one after another should produce the 'O' symbol. If there is not enough input data, or 'dots' and 'lines' are mixed, the network should not produce anything.

[a diagram of ANNA as a black box with 1s and 0s as the input, and Ss and Os as the output]

The following procedure will construct a network fulfilling all these requirements with nine asynchronous neurons plus one "dummy" input neuron, whose only purpose is to pass input further. But first let's describe how a single asynchronous neuron works.

# Neuron

Just as a standard artificial neuron, an asynchronous neuron basically sums signals coming from other neurons, checks if the sum is bigger than some *threshold* and if yes, it applies a *transfer function* and presents the result as its output to other neurons. In fact, in a synchronous network, where all the signals are available at once, we can bind the summing up and the treshold to the transfer function: the function may take all the inputs, sum it and if the sum is not big enough, the function will simply return 0.
This is not possible with asynchronous neurons. First of all, the signals are not available all in the same time. They are being sent to the neuron each in its own time. Also, please notice that the data flow is changed: A standard neuron looks for its input to other neurons and presents its own output for others to take. An asynchronous neuron is reactive: Another neuron sends a signal to it and it may or may not react to this event by computing the output and sending it to other neurons.
It means that in an asynchronous neuron we need a *buffer* which will hold signals sent by other neurons. After adding a new signal to the buffer, the neuron compares the sum with the threshold to check if it is big enough to trigger the transfer function. This is why it is more natural (although not entirely necessary) to hold the threshold separately from the transfer function.

There are also two additional fields: *hushValue* and *forgettingValue*.

### Being hushed

On top of sending numerical signals in the range of <0..1> a neuron can send a *hush* signal in order to request another neuron to stay silent for some time. The exact amount of time, counted in *ticks*, is decided by the *hushValue* of the neuron being hushed. In a special case the *hushValue* can be set to 0, meaning that the neuron will not respond to hush requests. A hushed neuron ignores incoming signals for the time being which makes hushing a very simple machanism for telling other neurons "you don't have to work on these signals anymore - I already processed them". This will become very useful in our example.

### Forgetting

The *forgettingValue* is another property working with *ticks*. It is a float number in the range <0..1> which tells the neuron how much it should "cool off" during one *tick*, ie.  how much of its buffer should be thrown away. If it is set to 0 the neuron will not forget anything from its buffer even if it has to wait for a long time for the next input signal. If it is set to 1, the whole buffer value will be reduced to 0 in one tick. Additionally, the *forgettingValue* may be set to a special *ForgetAll* value which means that the buffer will reset immediately after it being compared to the threshold. In this case the neuron will produce an output only if a single received signal is bigger than the threshold.
Implementation of the *forgettingValue* was inspired by the "cooling off" process observed in organic neurons. Its main purpose, used in our example, is to put the neuron into a state of waiting for additional input, but only for a given amount of time. If the signal does not come soon enough, the neurn goes back to its default state with the buffer being equal to 0. In effect, the *forgettingValue* is crucial for treating time gaps as information.

### Synapses

The last property of an ANNA neuron is a set of synapses. A synapse is a simple data structure consisting of a reference to another neuron and a *weight*. The *weight* can be a float number in the range <0..1>, or a special constant *Hush*. When then neuron fires, the output signal is multiplied by the synapse's weight and the result is then sent to the neuron at the other end of the synapse (remember the opposite direction of the data flow). If the synapse's "weight" is *Hush*, the signal is transformed into a hush request.

### Building blocks

A building block is the smallest group of interconnecteed neurons which can perform a given task. In the special case a single neuron is a building block by itself and the task it performs is to fire when it receives a signal of a certain value, or a certain sum of signals in a given time interval. A more complex example of a building block is what I called a Delay Gate (DG): a block whick after receiving a signal ignores all subsequent signals for a given time interval, and after that time it fires its own signal and starts accepting new signals again. DG consists of three neurons connected as on the diagram below. 

The neuron DG1 receives the input signal, immediately passes it to DG2 and hushes itself for N ticks. DG2 passes the signal further to DG3, but the weight of the synapse (DG2, DG3) is low - it is exactly N+1 times lower than the DG3's threshold, meaning that DG2 has to send the signal N+1 times (one immediately, then one during each tick). In order to achieve that, DG2 uses a synapse to itself (DG2, DG2). A signal sent through (DG2, DG2) comes to DG2 in the next tick and triggers another signal being sent from DG2, both to DG3 and again back to DG2.
In the meantime, DG3 simply waits for its buffer to sum enough signal values from DG2, which should happen exactly after N ticks. Then it fires its own signal to the outside of the block.
All the synapses in the block except from (DG2, DG3) have the weight = 1.0 which means that all the signals received from inside the block except from the signal received by DG3 from DG2 have the value = 1.0. We can control when the DG block is triggered by modifying DG1's threshold and forgettingValue, as well as weights of synpases connecting from outside of the block to DG1.

Another one is a Signal Sum (SS), consisting of two neurons: SS1 signals from the outside and if they are considerable enough it sends its own signal to SS2. The weight of the synapse (SS1, SS2) is exactly N times lower than the SS2's threshold, so SS2 wilfire only after receiving N such signals. The difference between an SS block and a single neuron is that with an SS block we can decide if the input signal is strong enough or not. A single asynchronous neuron simply accepts all the signals. 

These two building blocks are enough to build an S.O.S. network, but one more property is needed: A hushable building block is a building block with one of its neurons being a Hush neuron which, when triggered by any input, sends Hush signals to all other neurons in the block. Thus it works as a single point of entry for hushing signals from the other parts of the network. A neuron which wants to hush a hushable building block does not have to know its internals. It just has to know how to send a signal to the block's Hush neuron.

hDG - a hushable Delay Gate

hSS - a hushable Signal Sum

And now we can build the whole S.O.S network from one dummy input neuron, one hDG and three hSS:

In total, we used 9 standard, 4 hush and 1 input neurons.

==================================

1. What is it that I want to solve here
 - In the moment this project is not so much about solving a specific problem, but about investigating an idea - a type of artificial neural network (ANN) which works in real time and consists of neurons which are independent of one another and have more resemblance to organic neurons than mathematical models used in traditional ANNs. In time I plan to develop it to the point where such a network can be used as a data flow transformer - eg. receiving a stream of bytes and generating a stream of more abstract symbols, using as additional information both the context (ie. data which was received before) and time gaps between consecutive chunks of data.
 - A traditional Multi-Layered Perceptron (MLP) or a Hopfield model require to receive a whole input vector at once. Therefore, they are not well suited for a situation when the input signal appears gradually (a stream).
 - The structure of an artificial neural networks encourages us to treat each neuron as independent, but both MLP and Hopfield require synchronisation of neurons. In MLP we first compute the input layer, then the hidden, then the output. In Hopfield the order of computing is not important within one iteration (at least in the asynchronous version of the Hopfield model), but no neuron should be computed twice while another one is still awaiting its first computation.
 - At last, ANNs are built as very generic structures. In MLP every neuron of the N-th layer (N > 1) is connected to every neuron in the layer N-1. In Hopfield there is only one layer with all-to-all connections. Only when we teach the network we differentiate the weights of connections and in special case some of them may be set up to 0 (so, in practice, it's as if they didn't exist). Treating ANN as such it is possible to model it as a matrix with each cell (a,b) consisting of a weight of connection from the neuron a to b, and then computation of signals become simple multiplication of the matrix and the input vector. 
 - In contrast, my idea is not to focus on teaching a generic ANN, but to construct a small network performing a specific task using neurons which are smarter than the simple "sum the signals - check the threshold - compute the output" neurons of traditional models. Each neuron should be asynchronous, perform its own computations in order to achieve a specific effect and send the results only to a few other neurons which "know" what these results mean and how to deal with them.
2. An example: Recognizing the S.O.S. signal
 - to be continued...
