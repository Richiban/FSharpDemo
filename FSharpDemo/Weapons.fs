module Weapons

type AutomaticType = SemiAuto | BurstFire of int | FullAuto
type RepeaterType = Manual | Automatic of AutomaticType list
type BoltMode = OpenBolt | ClosedBolt
type RemovableMagazines = Box | Belt
type InternalMagazines = Tubular
type MagazineRemovableness = Removable of RemovableMagazines | Internal of InternalMagazines
type FeedType = 
    | MagazineFed of MagazineRemovableness * BoltMode
    | Cylinders of size : int
type FiringMode = SingleShot | MultiBarrelled of int | Repeater of RepeaterType * FeedType

type Weapon = {
    FiringMode : FiringMode
}

let Revolver = {
    FiringMode = Repeater (Automatic([SemiAuto]), Cylinders(6))
}

let Pistol = {
    FiringMode = Repeater (Automatic([SemiAuto]), MagazineFed(Removable(Box), ClosedBolt))
}

let SMG = {
    FiringMode = Repeater (Automatic([SemiAuto; BurstFire(2); FullAuto]), MagazineFed(Removable(Box), OpenBolt))
}

let SniperRifle = {
    FiringMode = Repeater (Manual, MagazineFed(Removable(Box), ClosedBolt))
}

let AssaultRifle = {
    FiringMode = Repeater (Automatic([SemiAuto; BurstFire(3); FullAuto]), MagazineFed(Removable(Box), ClosedBolt))
}

let GrenadeLauncher = {
    FiringMode = SingleShot
}

let PumpShotgun = {
    FiringMode = Repeater (Manual, MagazineFed(Internal(Tubular), ClosedBolt))
}

let DoubleBarrelledShotgun = {
    FiringMode = MultiBarrelled(2)
}

let MachineGun = {
    FiringMode = Repeater (Automatic([FullAuto]), MagazineFed(Removable(Belt), OpenBolt))
}

let load gun =
    match gun.FiringMode with
    | SingleShot -> printfn "Taking one round from pack and loading"
    | MultiBarrelled _ -> printfn "Taking two rounds from pack and loading"
    | Repeater (repeaterType, feedType) -> 
        ()