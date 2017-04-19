We thank the reviewers for their extensive reviews and thoughtful
suggestions.

1. **Formalization** We agree with reviewer A in that disparate
   formalisms to capture various aspects of VML is far from ideal. Our
   aim in building these formalizations is three-fold: (a). To give
   semantics to the VML primitives introduced in Sec. 2 and 3, (b). To
   show that the side condition on merge makes three-way merges
   possible by enforcing the unique LCA property, and (c). To prove
   that the side condition does not affect the progress guarantee of
   the system, i.e, every pair of branches can eventually satisfy the
   side condition, allowing them to merge (progress theorem is the
   relation between the runs of the machine and structure of the
   history graphs). We appreciate reviewer's suggestion to drop the
   abstract machine. Perhaps an inductively defined graph structure
   should suffice to prove the properties of interest.

2. **Formal model vs the implementation** Formal model does have a bearing
   on the implementation. The ring deployment described in Fig. 21(a),
   where merges happen in a circular order, satisfies the side
   condition on merges by construction. 

3. **Comparison with Concurrent Revisions (CRs)** Intuitively, the set
   of history graphs allowed under our model is a strict superset of
   revision diagrams. For instance, CR allows a branch to only merge
   with its parent, while VML allows merges between arbitrary
   branches. This generalization is useful in practice, since it
   admits a "server-less" fully decentralized system design required
   for highly-available web services. Both CR and VML admit three-way
   merges. CR gives a determinism guarantee, which is desirable in a
   concurrency model, but not so much in a distributed programming
   model since it is impossible to achieve without coordination
   (extensions of CR for distributed systems (ESOP'12, ECOOP'12)
   eschew determinism). Thus, CR for distributed systems is a
   specific instance of VML. We accept reviewer B's suggestion to
   include a performance comparison with CR.



