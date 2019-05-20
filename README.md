# nri-customers

Learning exercise with Haskell, Servant, Katip and Persistent.

The project consists of two parts, a web server which is still not finished, 
and a worker thread called, "daemon" in this case for a lack of a better name,
which acts as a kind of ETL process. The goal is for the daemon to extract some
data from an API and store in a PostgreSQL database, the webserver would act 
as a querying interface for the data (I'll probably add Esqueleto for the queries).

The `DaemonT` type is used as the basis for the dameon worker, it
implements all the necessary typeclass instances. 

The `ReqClient` typeclass which acts as a wrapper over the `Req` library, it 
was added as a way to mock http requests in tests.

Keeping in mind that this is a learning exercise, the plan is to mock the 
web server database calls using a free monad instead of the already
tried typeclass approach.

## Resources:

- https://haskell-servant.readthedocs.io/en/stable/index.html
- https://www.yesodweb.com/book/persistent
- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
- https://making.pusher.com/unit-testing-io-in-haskell/
- https://lexi-lambda.github.io/blog/2017/06/29/unit-testing-effectful-haskell-with-monad-mock/
- https://functor.tokyo/blog/2015-11-20-testing-db-access
- http://dev.stephendiehl.com/hask/#servant
