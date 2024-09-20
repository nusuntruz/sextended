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
- \ .Hook(cast, func, method)
- \ .unHook(method)
- \ .unHookAll()

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
- GetPlayerByUserId(m_iUserID)
- GetViewAngles()
- SetViewAngles(vec3_t_Angles)
- Execute(client_cmd_execute_command)
- IsPaused()
- IsHLTV()
- GetScreenAspectRatio(viewportWidth, viewportHeight)
- GetDemoPlaybackParameters()
- \ .m_uiCaseID
- \ .m_uiHeaderPrefixLength
- \ .m_uiLockFirstPersonAccountID
- \ .m_bAnonymousPlayerIdentity
- \ .m_numRoundSkip
- \ .m_numRoundStop
- \ .m_bSkipWarmup
- \ .m_bPlayingLiveRemoteBroadcast
- \ .m_uiLiveMatchID
- GetPlayerInfo(m_iIndex)
- \ .m_uDataMap 
- \ .m_iSteamID64 
- \ .m_iSteamIDLow 
- \ .m_iSteamIDHigh 
- \ .m_szName 
- \ .m_iUserID 
- \ .m_szGUID 
- \ .m_uFriendID 
- \ .m_szFriendName 
- \ .m_bIsFakePlayer 
- \ .m_bIsHLTV 
- \ .m_uCustomFiles 
- \ .m_uFilesDownloaded

NetChannel:
- m_bProcessingMessages
- m_bShouldDelete
- m_nOutSequenceNr
- m_nInSequenceNr
- m_nOutSequenceNrAck
- m_nOutReliableState
- m_nInReliableState
- m_nChokedPackets

NetGraph:
- m_FrameRate
- m_AvgLatency
- m_AvgPacketLoss
- m_AvgPacketChoke
- m_IncomingSequence
- m_OutgoingSequence
- m_UpdateWindowSize
- m_IncomingData
- m_OutgoingData
- m_AvgPacketIn
- m_AvgPacketOut
- m_hFontProportional
- m_hFont
- m_nNetGraphHeight

Entity:
- GetLocalPlayer() -- as index
- Get(m_iIndex)
- GetHandle(m_iHandle)
- Find(m_hAddress) -- By Address
- \ :GetProp(typedef, m_hOffset, m_hPointer)
- \ :m_iHealth()
- \ :m_iTeamNum()
- \ :m_nPoseParameters()
- \ :m_bDormant()
- \ :m_fFlags()
- \ :m_vecVelocity()
- \ :m_flVelocity()
- \ :m_flSimulationTime()
- \ :m_flOldSimulationTime()
- \ :m_flLowerBodyYawTarget()
- \ :m_nAnimationLayers()[1 -> 12] -- {LayerNumber}
- \\\ .m_flAnimationTime
- \\\ .m_flFadeOutTime
- \\\ .m_iFlags
- \\\ .m_iActivity
- \\\ .m_iPriority
- \\\ .m_iOrder
- \\\ .m_iSequence
- \\\ .m_flPrevCycle
- \\\ .m_flWeight
- \\\ .m_flWeightDeltaRate
- \\\ .m_flPlaybackRate
- \\\ .m_flCycle
- \\\ .m_nOwner
- \\\ .m_iBits
- \ :m_nAnimationState()
- \\\ .m_nEntity
- \\\ .m_nActiveWeapon
- \\\ .m_nLastActiveWeapon
- \\\ .m_flLastUpdateTime
- \\\ .m_iLastUpdateFrame
- \\\ .m_flLastUpdateIncrement
- \\\ .m_flEyeYaw
- \\\ .m_flEyePitch
- \\\ .m_flGoalFeetYaw
- \\\ .m_flLastFeetYaw
- \\\ .m_flMoveYaw
- \\\ .m_flLastMoveYaw
- \\\ .m_flLeanAmount
- \\\ .m_flFeetCycle
- \\\ .m_flMoveWeight
- \\\ .m_flMoveWeightSmoothed
- \\\ .m_flDuckAmount
- \\\ .m_flHitGroundCycle
- \\\ .m_flRecrouchWeight
- \\\ .m_vecOrigin
- \\\ .m_vecLastOrigin
- \\\ .m_vecVelocity
- \\\ .m_vecVelocityNormalized
- \\\ .m_vecVelocityNormalizedNonZero
- \\\ .m_flVelocityLenght2D
- \\\ .m_flJumpFallVelocity
- \\\ .m_flSpeedNormalized
- \\\ .m_flRunningSpeed
- \\\ .m_flDuckingSpeed
- \\\ .m_flDurationMoving
- \\\ .m_flDurationStill
- \\\ .m_bOnGround
- \\\ .m_bHitGroundAnimation
- \\\ .m_flNextLowerBodyYawUpdateTime
- \\\ .m_flDurationInAir
- \\\ .m_flLeftGroundHeight
- \\\ .m_flHitGroundWeight
- \\\ .m_flWalkToRunTransition
- \\\ .m_flAffectedFraction
- \\\ .m_flMinBodyYaw
- \\\ .m_flMaxBodyYaw
- \\\ .m_flMinPitch
- \\\ .m_flMaxPitch
- \\\ .m_iAnimsetVersion

Input:
- IsKeyPressed(vKey)
- IsKeyToggled(vKey)
- GetCursorPosition()
- GetScroll()

Render:
- Font(name, h, weight, flags, blur)
- \ :Measure(text)
- \ :Text(pos, clr, text, ...) -- Color(255, 255, 0), "next text", "another text"
- MassCreateFont(name, h_min, h_max, weight_min, weight_max, flags)
- Polygon(vertices, clipvertices, clr)
- Polyline(vertices, clr)
- SetClip(vec, size)
- EndClip()
- Line(pos1, pos2, clr)
- FilledRect(pos, dim, clr, clr2, horizontal, rounding, flags)
- Rect(pos, dim, clr, clr2, horizontal, rounding, flags)
- Circle(pos, radius, color, start_angle, end_angle)
- FilledCircle(pos, radius, color, start_angle, end_angle)
- Circle3D(Position, flRadius, Color)
- InitTexture(data, w, h)
- Image(datatbl, pos, dim, alpha)

vec2_t, vec3_t:
- vec2_t(x, y, z) or vec2_t({x, y, z}) -- Third parameter for world to screen
- vec3_t(x, y, z) or vec3_t({x, y, z})
- \ :ffi()
- \ :Add(vec or number)
- \ :Sub(vec or number)
- \ :Multiply(vec or number)
- \ :Fraction(vec or number)
- \ :Dot(vec)
- \ :Length()
- \ :Length2d()
- \ :Distance(number)
- \ :Angle(vec)
- \ :Cross(vec3d)
- \ :unpack()
- \ :Hovered(vec2d)
- \ :Drag(vec2d, m_szName, m_bShouldBlockDragging)

Color:
- Color(r, g, b, a)
- Color() or Color(255, 255, 255) or Color(255, 255, 255, 255) -- White
- \ :toHSV()
- \ :toHEX()
- \ :SetAlpha(alpha)
- \ :Print(spacing)
- \ :Lerp(new_color, fraction)
- \ :unpack()

HSV:
- HSV(360, 1.0, 1.0)
- \ :toRGB()

HEX:
- HEX("#FFFFFFFF")
- \ :toRGB()

Callbacks:
- CreateMove(func(CUserCMD))
- Draw(func) -- surface
- FrameStage(func(m_iStage))
- AnimationUpdate(func(m_nEntity, UpdateAnimations()))

