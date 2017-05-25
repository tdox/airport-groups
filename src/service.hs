module Service where

{-
Airport sets micro service

GET /run_program
Accepts text of airport set program. Compiles and runs.
Returns the output as text.

POST /save_program
Accepts text of airport set program. Compiles and runs.
Returns either compiler error or id of new set of sets (sets_id). Also returns the output of running the program. This set of sets is associated with some verified user / token.

GET /set?sets_id={sets_id}&set_name={set_name}
Returns either an error message or a JSON list of airport codes in the set

GET /is_airport_in_set?sets_id={sets_id}&set_name={set_name}&airport_code={airport_code}
An airport code is a string such as “FAA:SFO” or “ICAO:KTEB”
Returns true if the airport is in the set

GET /does_airport_satisify_predicate?sets_id={sets_id}&predicate_name={predicate_name}&airport_code={airport_code}
Returns true if the airport satisfies the predicate.

-}
