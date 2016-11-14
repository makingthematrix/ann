Artificial Neural Networks in Akka
==================================

1. Premise

  This project is an attempt to use an artificial neural network as a data flow transformer. Having an input stream of symbols which can be decoded into an input vector, the network will be able to generate a stream of more abstract symbols, using as additional information both the context (ie. data which was received before) and time gaps between consecutive chunks of data. The network is reactive - its computations are triggered by the input stream itself, not by a third agent watching the stream and sending requests to the network - and it can simultaneously receive and generate data.
  
2. Documentation
  * [Abstract]
  * [Full article]
  * [Install and use instructions]
  * [Further ideas]

If you want to look at only one class in this project  then it's probably [this one](https://github.com/makingthematrix/ann/blob/SOSWithBlock_1.0/src/main/scala/anna/async/Neuron.scala).


   [Abstract]: <https://github.com/makingthematrix/ann/blob/SOSWithBlock_1.0/doc/ArtificialNeuralNetworksInAkka-abstract.pdf>
   [Full article]: <https://github.com/makingthematrix/ann/blob/SOSWithBlock_1.0/doc/ArtificialNeuralNetworksInAkka.pdf>
   [Install and use instructions]: <https://github.com/makingthematrix/ann/blob/SOSWithBlock_1.0/doc/InstallAndUseInstructions.pdf>
   [Further ideas]: <https://github.com/makingthematrix/ann/blob/SOSWithBlock_1.0/doc/FurtherIdeas.pdf>
