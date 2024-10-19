# Gibbon Benchmarks

## CRDTs
- [ ] Set Type - Add, Remove, Merge
- [ ] Register Type - Set, Merge
- [ ] Ordered List - Insert, Remove, Merge

## Benchmarking
**Things to control for:**
- Payload Size
- Document Size 
- Merge Complexity (adding one 12 char block != adding 12 one char blocks)

**Measurements:**

Tests will be carried out (ideally) in their native language. Measurements can be written to an output file and then analyzed in python. We'll need to control for the network; a solution might be to use another intermeriate (like a file). It will undoubtedly be intersting to test on a live network of course.

Name | Description | Network | Method
---- | ----------- | ------- | ------
Streaming | High frequency, small simple payload | X | Stream individual updates into a remote document, measure time from sent -> response
Merging | Low Frequency, large complex payload | X | Execute a full handshake and exchange between two documents. The payload should have multiple discrete edits
Scaling | Test operations on documents of varying size | | Measure the time it takes to implement changes on documents of varying size. Both in # of uuids and raw content.
Granular | Test individual operations in isolation | | Measure the time it takes individual operations to complete. Use this as a baseline.

**Evaluation**

The Gibbon implementation will be compared to:
- A ghc version of the same code
- A C implementation of similair sophistication
- Yjs: a javascript implementation of the same algorithm, state of art CRDT library. 


## Timeline
- Week [9/13/23]: Internal Libraries (Set and Map) & Benchmarking framework
- Week [9/20/23]: CRDT Set & Register (naive) - test benchmarking framework <- modules would be nice to have here
- Week [9/27/23]: CRDT Ordered List (naive)
- Week [10/4/23]: CRDT Set & Register (optimized)
- Week [10/11/23]: CRDT Ordered List (optimized)
- Week [10/18/23]: Agressive Benchmarking <- network primitives would be nice to have here
- Week [10/25/23]: XML