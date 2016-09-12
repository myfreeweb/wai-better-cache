# wai-better-cache

It's, like, a way better cache for [WAI].

- Controlled by the application, using the Cache-Control header and the Vary header (TODO: and its proposed superset, the [Key header])
  - Yes, the same headers that are respected by web browsers' caches! Configure server-side caching, get client-side for free (or vice versa)
  - By the way, [Caching best practices] is a great article about setting up the caching headers
- Pluggable storage backends. Included:
  - in-memory [lrucache](https://github.com/chowells79/lrucache)
- A cache uses two separate backends (instances): one for primary entries (map: URL without the query → key generator, i.e. the Vary/Key header) and one for secondary entries (map: result of applying the key generator to the request → cached response)
- A great test suite

[WAI]: https://hackage.haskell.org/package/wai
[Key header]: https://tools.ietf.org/html/draft-ietf-httpbis-key-01
[Caching best practices]: https://jakearchibald.com/2016/caching-best-practices/

## Security

A cache can have both positive and negative security impact.

If your website does expensive operations like contacting the database or the filesystem and rendering templates on every request (I'm guilty of *calling out to a JavaScript interpreter* for templating in [Sweetroll], hehe), [Application Denial of Service (DoS)] attacks — basically, a random person overloading your server — become very easy.

But if all unauthenticated requests are quickly answered from RAM (which is what this library should help you with), all authenticated requests are throttled (e.g. with [wai-middleware-throttle]) and authentication is performed instantly (e.g. validating a [JSON Web Token], which is just checking a [MAC] or signature, no database requests or anything), your app should survive very high traffic loads.

However, you should carefully consider the interaction between caching and authorization.
Remember [the Steam incident]?
While they were fighting DoS attacks, they made a mistake in their caching rules, and people were seeing pages intended for other users!
With wai-better-cache, hopefully, this kind of mistakes will be harder to make, because the rules are set with HTTP headers, right in the application code that answers requests — instead of a completely separate place like [a special configuration language].
But still, you have to be careful ;-)

So, when using a cache for security:

- cache ALL THE THINGS!
- No, really. Even the most dynamic content. Cache it for one second. That will matter if you get a thousand requests in one second!
- Work on your key generation. That's the `Vary`/`Key`/`Vary-Query` stuff. If an attacker can bypass the cache by requesting `/page?_randomUnsupportedParameter=RANDOM_STRING__HERE_EVERY_TIME`, it's no good. This is why wai-better-cache goes beyond current standards and accepts a custom `Vary-Query` header. Which works exactly like `Vary`, but for the query string instead of headers.
  - But what if the attacker uses random crap like `/?page=123123213213123`, so, a supported parameter but a random value? Well… you're on your own here :-( Make a cache for valid ranges inside of your app, or something. Just make sure that you fail fast and return an error code. wai-better-cache only caches `2xx` responses.
- Configure caching of auth-protected content correctly. `Cache-Control: no-store` or `Vary: Authorization` or something.

[Sweetroll]: https://github.com/myfreeweb/sweetroll
[Application Denial of Service (DoS)]: https://www.owasp.org/index.php/A9_2004_Application_Denial_of_Service
[wai-middleware-throttle]: https://hackage.haskell.org/package/wai-middleware-throttle
[JSON Web Token]: https://jwt.io
[MAC]: https://en.wikipedia.org/wiki/Message_authentication_code
[the Steam incident]: http://store.steampowered.com/news/19852/
[a special configuration language]: https://www.varnish-cache.org/docs/4.1/users-guide/vcl.html

## Contributing

Please feel free to submit pull requests!

By participating in this project you agree to follow the [Contributor Code of Conduct](http://contributor-covenant.org/version/1/4/) and to release your contributions to the public domain / under the UNLICENSE.

[The list of contributors is available on GitHub](https://github.com/myfreeweb/wai-better-cache/graphs/contributors).


## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).
