#!/usr/bin/env python

import asyncio


# define a coroutine function, which returns a coroutine object
@asyncio.coroutine
def delayed_result(delay, result):
    yield from asyncio.sleep(delay)
    print(result)
    return result

# get an event loop
loop = asyncio.get_event_loop()

x = loop.run_until_complete(delayed_result(1.5, 23))
