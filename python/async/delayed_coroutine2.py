#!/usr/bin/env python

import asyncio

# this example asyncio uses the new async and await syntax
# 1) instead of @asyncio.coroutine we use the async keyword to mark coroutine
# functions
# 2) instead of using 'yield from' to give back control to the event loop,
# we use the await keyword


# define a coroutine function, which returns a coroutine object
async def delayed_result(delay, result): 
    await asyncio.sleep(delay)
    print(result)
    return result

# get an event loop
loop = asyncio.get_event_loop()

x = loop.run_until_complete(delayed_result(1.5, 23))

