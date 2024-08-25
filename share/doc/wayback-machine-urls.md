Wayback Machine URL reference
================================================================================


Examples
--------------------------------------------------------------------------------
List everything captured for a domain:

	https://web.archive.org/web/*/http://site.domain/*

Direct link to original (unmodified) file:

	date=`date +%Y%m%d`
	https://web.archive.org/web/${DATE}id_/http://site.domain/page.html

Newest and oldest versions of an archived page, respectively:

	Latest: https://web.archive.org/web/${URL}
	Oldest: https://web.archive.org/web/1000/${URL}

JSON API for interrogating a resource's archive status:

	curl "https://archive.org/wayback/available?url=$URL" \
	| JQ_COLORS= jq -Mre '.archived_snapshots.closest.url | select(. != null)'



Syntax
--------------------------------------------------------------------------------
Resources archived by [web.archive.org][] have the following format:

<pre>https://web.archive.org/web/${<a
href="#date">DATE</a>}${<a
href="#flags">FLAGS</a>}/${<a
href="#url">URL</a>}</pre>


### <a name="date">`${DATE}`</a>
`${DATE}` is a timestamp in `YYYYMMDDhhmmss` format (e.g., `date +%Y%m%d`). Date
components are optional – omitting them redirects to the chronologically-closest
snapshot. For example:

	https://web.archive.org/web/202204/https://atom.io/docs
	https://web.archive.org/web/20220501233408/https://atom.io/docs

Appending `*` to an incomplete timestamp will return a list of URLs matching the
provided date components. A lone `*` is also valid and returns all captures ever
recorded for [`${URL}`](#url).

If the `${DATE}/` segment is omitted altogether, the most recently-saved version
of a resource will be returned. For example, both of these URLs are equivalent:

	https://web.archive.org/web/https://www.wikipedia.org/
	https://web.archive.org/web/9999/https://www.wikipedia.org/


### <a name="flags">`${FLAGS}`</a>
`${FLAGS}` are zero or more [URL rewrite modifiers][1], each of which are:

<!----------------------------------------------------------------------------->
| Flag  | Definition                                                           |
|-------|----------------------------------------------------------------------|
| `id_` | Identity (no URL modification); return original capture verbatim     |
| `js_` | Rewrite URLs using JavaScript parser                                 |
| `cs_` | Rewrite URLs using CSS parser                                        |
| `im_` | Hint that resource is used as an image                               |
| `fr_` | Hint that resource is used inside a [`<frame>`]                      |
| `if_` | Hint that resource is used inside an [`<iframe>`]                    |
| `oe_` | Hint that resource is used inside an [`<object>`] or [`<embed>`] tag |
| `cm_` | Charset detection mode; an integer specifies strategy (e.g., `cm1_`) |
| `fw_` | Frame-wrapped context. Analogous to `fr_`                            |
| `mp_` | Main page context. Analogous to `if_`                                |
<!----------------------------------------------------------------------------->


### <a name="url">`${URL}`</a>
The URL of the original resource. A leading or trailing `*` will cause a list of
results matched by prefix or suffix, respectively.

HTTP and HTTPS URLs are matched interchangeably by queries. Moreover, the latter
protocol will be used for URLs that lack a scheme component; i.e., the following
URLs point to the same resource:

	https://web.archive.org/web/www.wikipedia.org/
	https://web.archive.org/web/https://www.wikipedia.org/


<!-- Referenced Links --------------------------------------------------------->
[`<embed>`]:  https://mdn.io/HTML/embed
[`<frame>`]:  https://mdn.io/HTML/frame
[`<iframe>`]: https://mdn.io/HTML/iframe
[`<object>`]: https://mdn.io/HTML/object
[web.archive.org]: https://web.archive.org/
[1]: http://pywb.readthedocs.io/en/docs/manual/rewriter.html#urlrewrite-type-mod
