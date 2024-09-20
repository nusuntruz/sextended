SeaSide Extended API:

Whats sextended?
Sextended is a library that creates a "translation layer" between CSGO Cheats Lua APIs.

What does that mean?
In short terms, any script that uses this library can be loaded on any cheat (If the creator knows how to use this).

Utils:
- FindSignature(module_dll, sequence, offset)
- ProcessBind(typedef, m_szModuleNameDll, m_szFunctionName)
- CreateInterface(module_dll, interface_name)
- Print(...) -- Color(255, 255, 255), "Some Text", "Another Text", Color(255, 0, 0), "Another Text But Red" ...
- ChatPrint(m_szText)
- SafeError(m_szFunction, m_szError, ret)
- Error(m_szFunction, m_szError, m_iLevel)
- VTableBind(interface, m_iIndex, typedef)
- VirtualFunction(vTable, m_iIndex, typedef)

Clipboard:
- Get()
- Set(m_szText)

Hooks:
- Create(m_nHookAddress)
\ .Hook(cast, func, method)
\ .unHook(method)
\ .unHookAll()

ClientState:
- m_nChallengeNr
- m_nSignonState
- m_flNextCmdTime
- m_nServerCount
- m_nCurrentSequence
- m_nDeltaTick
- m_bPaused
- m_nViewEntity
- m_nPlayerSlot
- m_szLevelName
- m_szLevelNameShort
- m_szGroupName
- m_iMaxClients
- m_iLastServerTickTime
- m_bInSimulation
- m_iOldTickcount
- m_flTickRemainder
- m_flFrameTime
- m_iLastOutGoingConnect
- m_iChockedCommands
- m_iLastCommandAck
- m_iLastServerTick
- m_iCommandAck
- m_nSoundSequence
- m_vecViewAngles
- m_nEvents

Engine:
- InGame()
- IsConnected()
- GetScreenSize()
- GetPlayerInfo(m_iIndex)
\ .m_uDataMap
\ .m_iSteamID64
\ .m_iSteamIDLow
\ .m_iSteamIDHigh
\ .m_szName
\ .m_iUserID
\ .m_szGUID
\ .m_uFriendID
\ .m_szFriendName
\ .m_bIsFakePlayer
\ .m_bIsHLTV
\ .m_uCustomFiles
\ .m_uFilesDownloaded
- GetPlayerByUserId(m_iUserID)
- GetViewAngles()
- SetViewAngles(vec3_t_Angles)
- Execute(client_cmd_execute_command)
- IsPaused()
- IsHLTV()
- GetDemoPlaybackParameters()
- GetScreenAspectRatio(viewportWidth, viewportHeight)

NetChannel:
- m_bProcessingMessages
- m_bShouldDelete
- m_nOutSequenceNr
- m_nInSequenceNr
- m_nOutSequenceNrAck
- m_nOutReliableState
- m_nInReliableState
- m_nChokedPackets

Entity:
- GetLocalPlayer() -- as index
- Get(m_iIndex)
- GetHandle(m_iHandle)
- Find(m_hAddress) -- By Address
\ :GetProp(typedef, m_hOffset, m_hPointer)
\ :m_iHealth()
\ :m_iTeamNum()
\ :m_nAnimationLayers()
\\ [1 -> 12] -- {LayerNumber}
\\\ .m_flAnimationTime
\\\ .m_flFadeOutTime
\\\ .m_iFlags
\\\ .m_iActivity
\\\ .m_iPriority
\\\ .m_iOrder
\\\ .m_iSequence
\\\ .m_flPrevCycle
\\\ .m_flWeight
\\\ .m_flWeightDeltaRate
\\\ .m_flPlaybackRate
\\\ .m_flCycle
\\\ .m_nOwner
\\\ .m_iBits
\ :m_nAnimationState()
\\ .m_nEntity
\\ .m_nActiveWeapon
\\ .m_nLastActiveWeapon
\\ .m_flLastUpdateTime
\\ .m_iLastUpdateFrame
\\ .m_flLastUpdateIncrement
\\ .m_flEyeYaw
\\ .m_flEyePitch
\\ .m_flGoalFeetYaw
\\ .m_flLastFeetYaw
\\ .m_flMoveYaw
\\ .m_flLastMoveYaw
\\ .m_flLeanAmount
\\ .m_flFeetCycle
\\ .m_flMoveWeight
\\ .m_flMoveWeightSmoothed
\\ .m_flDuckAmount
\\ .m_flHitGroundCycle
\\ .m_flRecrouchWeight
\\ .m_vecOrigin
\\ .m_vecLastOrigin
\\ .m_vecVelocity
\\ .m_vecVelocityNormalized
\\ .m_vecVelocityNormalizedNonZero
\\ .m_flVelocityLenght2D
\\ .m_flJumpFallVelocity
\\ .m_flSpeedNormalized
\\ .m_flRunningSpeed
\\ .m_flDuckingSpeed
\\ .m_flDurationMoving
\\ .m_flDurationStill
\\ .m_bOnGround
\\ .m_bHitGroundAnimation
\\ .m_flNextLowerBodyYawUpdateTime
\\ .m_flDurationInAir
\\ .m_flLeftGroundHeight
\\ .m_flHitGroundWeight
\\ .m_flWalkToRunTransition
\\ .m_flAffectedFraction
\\ .m_flMinBodyYaw
\\ .m_flMaxBodyYaw
\\ .m_flMinPitch
\\ .m_flMaxPitch
\\ .m_iAnimsetVersion
\ :m_nPoseParameters()
\ :m_bDormant()
\ :m_fFlags()
\ :m_vecVelocity()
\ :m_flVelocity()
\ :m_flSimulationTime()
\ :m_flOldSimulationTime()
\ :m_flLowerBodyYawTarget()

Input:
- IsKeyPressed(vKey)
- IsKeyToggled(vKey)
- GetCursorPosition()
- GetScroll()

Callbacks:
- CreateMove(func(CUserCMD))
- Draw(func) -- surface
- FrameStage(func(m_iStage))
- AnimationUpdate(func(m_nEntity, UpdateAnimations()))

