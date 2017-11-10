# New Calendar-Based Six-Monthly Release Schedule


>
>
> **This page is to facilitate and summarise the GHC DevOps group discussion around a calendar-based six monthly release schedule. Once, the discussion has finished and the new schedule been implemented all relevant information from this page should be absorbed into the parent page.**
>
>

## Proposed timeline


- Cut release branch: T - 3 month
- Alpha release: shortly after release branch cut
- First beta release: two weeks after alpha
- Second, third & forth beta releases : every fortnight
- RC1: T - 4 weeks
- RC2: T - 2 weeks


Pre-release stability


- *Alpha* release: This is a newly cut release branch that is guaranteed to build on all Tier 1 platforms and passes fast validation. Beyond that, no guarantees: some features slated for this release may not have fully landed.
- *Beta* release: The release has seen some testing beyond `./validate`. Only previously agreed on, stable and tested new functionality is allowed in. The focus is on bug fixing.
- *Release candidate (RC)* release: Strictly no new functionality. Only bug fixes are allowed.

## Challenges and points for discussion


### Impact on libraries



Currently, many libraries have a three GHC versions support policy: [
https://prime.haskell.org/wiki/Libraries/3-Release-Policy](https://prime.haskell.org/wiki/Libraries/3-Release-Policy). If there are more frequent GHC versions, this may lead to shorter support times of these libraries.


- This is seen by some as an argument against six-monthly GHC releases.
- We could ask the Core Libraries Committee to consider a change to supporting more than three GHC versions.
- Alternatively, we could ask the Core Libraries Committee to add a clause to the effect that stability will be maintained for at least a fixed absolute time frame (e.g., two or three years). This would make this policy less fragile with respect to changes in GHC release times.

### Impact on feature turn around



The speed with which new features are added to GHC is mostly limited by people willing and able to perform the required implementation work and also by the GHC Steering Committee's willingness to accept languages proposals. Hence, more frequent releases at a predictable schedule won't, by itself, increase the rate of new features getting into GHC, but it will lead to a more gentle feature introduction (fewer features per release), and hence, hopefully also smoother adoption of new GHC versions.



Equally importantly, it will make the feature release schedule more predictable for features authors who, due to the more frequent releases, will hopefully see less need to get insufficiently tested code into a release at the last minute — with all the potential destabilising effects that that can have. In fact, the release manager should use the accelerated release schedule as an argument to actively discourage last minute additions.



Moreover, with more frequent major release, we should have a complete ban on backwards incompatible changes in point releases.



NB: Only during the RC phase, there is a complete ban on new functionality. Hence, a feature that doesn't make it into a release before that time will have to wait 7 months before it can possibly be released. This is the worst case scenario, which compares very favourably to the previous 12 to 14 month worst case.


### General ecosystem impact



While changing the GHC release frequency, we must have a close eye on how long it takes the ecosystem to catch up to GHC releases. With the current 2-4 months of catching up, six-monthly release may well create too much overhead. We expect that the more gentle feature introduction and better testing with the new CI infrastructure reduces the time for the ecosystem to catch up to a new GHC version; if this is not the case, we may have to reconsider our approach.


## First instance: GHC 8.4



The first release under the new scheme will be GHC 8.4.



*Timeline*


- Nov '17: cut release branch
- Feb '18: **release**

## Second instance: GHC 8.6


- **Release date:** August 2018
- Release branch creation: May 2018
