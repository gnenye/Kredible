# Kredible

## Overview

Kredible is a decentralized reputation system that lets users build, earn, and manage reputation across domains while preserving privacy and user control.

## Features

* Create and manage credibility sectors
* Endorse participants with weighted recommendations
* Record and verify engagements and activities
* Add verifications and delegated validation providers
* Delegate reputation management to representatives
* Configure visibility and privacy preferences
* Reputation decay over time for inactivity

## Data Structures

* `credibility-sectors`: Sector metadata and weighting parameters
* `participant-credibility`: Participant scores, counts, validation level, and decay settings
* `recommendations`: Endorsements with strength, notes, and skill areas
* `validations`: Verification records and tiers
* `credibility-engagements`: Recorded activities and confirmations
* `authorized-validation-services`: Delegated verification providers
* `credibility-delegations`: Delegation records for representatives
* `visibility-preferences`: Privacy controls and permitted viewers
* `next-sector-id`, `next-engagement-id`: ID counters

## Key Functions

* `create-credibility-sector(label, summary, recommendation-weight, engagement-weight, validation-weight, min-recommendations-required)`
  Create a new reputation domain.

* `recommend-participant(sector-id, recommended, strength, note, skill-areas)`
  Endorse a participant and update recommendation count.

* `remove-recommendation(sector-id, recommended)`
  Disable an endorsement and update counts.

* `record-engagement(sector-id, engagement-category, magnitude, content-hash)`
  Log an activity and increment engagement count.

* `verify-engagement(sector-id, engagement-id)`
  Confirm an activity by sector manager or authorized validator.

* `add-validation(sector-id, participant, validation-category, proof-hash, tier, expires-when)`
  Add a verification entry and update participant validation level.

* `revoke-validation(sector-id, participant, validation-category)`
  Revoke a verification and recalculate validation tier.

* `delegate-credibility(sector-id, representative, expires-when)`
  Delegate reputation management rights.

* `remove-delegation(sector-id)`
  Revoke a delegation.

* `add-validation-service(sector-id, service, service-name, validation-categories)`
  Register an authorized validation provider.

* `revoke-validation-service(sector-id, service)`
  Revoke a provider.

* `update-visibility-preferences(sector-id, visible-score, visible-recommendations, visible-engagements, visible-validations, permitted-viewers)`
  Configure privacy and permitted viewers.

* Read-only: `get-sector-details`, `get-credibility-score`, `get-participant-recommendations`, `get-validation`, `is-service-authorized`, `get-visibility-preferences`

## Scoring Model

* Component scores range 0-1000:

  * Recommendation score based on endorsement count and min requirement
  * Engagement score based on recorded activity magnitude and counts
  * Validation score scaled by verification tier
* Sector weights apply to components and sum to 100 or less
* Composite score is weighted sum, then decay applied based on inactivity and deterioration rate

## Privacy and Access

* Participants control visibility of scores, endorsements, engagements, and validations
* Permitted viewers list grants granular access to private data
* Owners can always view their own data

## Usage Flow

1. Create a credibility sector with weights and requirements.
2. Users earn endorsements, record engagements, and obtain validations.
3. Managers or authorized services verify engagements and validations.
4. Scores are recalculated on updates and decay over time if inactive.
5. Participants set visibility preferences or delegate management as needed.
