# Test suite for team extension

## Test 1 - test abort agent

I need to test the team_abort tool, and for that we should:

1. start a new scout agent with a dummy task, no write operations
2. call the team_abort tool with the scout agent's session ID
3. verify that the tool returns the ID of the aborted agent

## Test 2 - test about multiple agents but not all

We need to test the team_abort tool with multiple agents, but not all of them.

1. start three agents with dummy tasks, no write operations
2. call the team_abort tool with two of the agents' session ID
3. verify that the tool returns the ID of the aborted agent
4. wait on the third agent to complete

## Test 3 - test about all agents

We need to test the team_abort tool with all agents.

1. start three agents with dummy tasks, no write operations
2. call the team_abort to abort all agents
3. verify that the tool returns the ID of the aborted agent
4. check if no agents are running

## Test 4 - test respond to waiting interactive agent

We need to test the team_respond tool with a waiting interactive agent.

1. start an interactive agent with a dummy task that asks it to wait for follow-up input, no write operations
2. wait until the agent reports a waiting status
3. call the team_respond tool with the waiting agent's session ID and a follow-up message
4. verify that the tool returns a success message containing the same agent ID
5. verify that the agent resumes and returns a new steering message that uses the follow-up message
6. verify that the agent is no longer listed as waiting after it completes

## Test 5 - test respond to non-waiting agent

We need to test the team_respond tool rejects agents that are not waiting.

1. start a non-interactive scout agent with a dummy task, no write operations
2. before it completes, call the team_respond tool with the scout agent's session ID and any message
3. verify that the tool returns an error saying the agent is not waiting for a response
4. let the scout agent complete or abort it if it is still running

## Test 6 - test respond to missing agent

We need to test the team_respond tool rejects unknown agent IDs.

1. call the team_respond tool with a made-up agent ID and any message
2. verify that the tool returns an error saying there is no subagent with that ID

## Test 7 - test done with waiting interactive agent

We need to test the team_done tool closes a waiting interactive agent.

1. start an interactive agent with a dummy task that asks it to wait for follow-up input, no write operations
2. wait until the agent reports a waiting status
3. call the team_done tool with the waiting agent's session ID
4. verify that the tool returns a success message containing the same agent ID
5. call team_list and verify that the closed agent is no longer listed as active or waiting
6. verify that no new steering message is delivered for that closed agent
