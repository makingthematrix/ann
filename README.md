Artificial Neural Networks in Akka
==================================

1. Premise

  This project is an attempt to use artificial neural network as a data flow transformer. Having an input stream of symbols which can be decoded into an input vector, the network will be able to generate a stream of more abstract symbols, using as additional information both the context (ie. data which was received before) and time gaps between consecutive chunks of data. The network is reactive - its computations are triggered by the input stream itself, not by a third agent watching the stream and sending requests to the network - and it can simultaneously receive and generate data.

2. Notes about the implementation

  I'm using Scala & Akka. Each neuron is implemented as an actor and the implementation differs in many ways and is more complex than from the traditional "sum the signals - check the threshold - compute the output" model. Because of increased complexity of a neuron the network as a whole may be composed of much less neurons than in MLP or Hopfield and achieve similar results. On the other hand, teaching it is more difficult. The network can be constructed by hand from "blocks" of small number of neurons interacting with each other in order to perform specific tasks, but it doesn't have a generic structure (as, for example, layers in the MLP model, where each neuron of the Nth layer is connected to all neurons of the N-1 layer) and there is no generic teaching algorithm, as a back-propagation algorithm in MLP. Instead, I plan to implement an evolutionary programming engine which, given a set of unit and performance tests, would contruct a network best suited to pass these tests. Since in this model there is much less neurons and connections between them, each change will have a much greater effect on the network's results than it is the case with traditional models, which hints that this approach may be successful.

3. How can it be useful?
  - Detecting information in noised data streams; an example implemented here is an S.O.S. signal which is received by the network as short bursts of signal (1), with time gaps between them (0). 
  - Voice or music recognition; a network constructed to recognize a characteristic fragment of a melody can then be used to search for covers and plagiarisms.
  - Making quick decisions in a real-world situations with incomplete data received not as one big chunk, but constantly through time; eg. a drone being able to avoid an approaching object during the flight without having to pass the warning to the controller and waiting for the answer.
  - Convincing one of AI research facilities in the world to work with me on this, or similar projects.

If you want to look at only one class in this project  then it's probably [this one](https://github.com/makingthematrix/ann/blob/SOSWithBlock_1.0/src/main/scala/anna/async/Neuron.scala).
