{ $Header: /MidiComp/CIRCBUF.PAS 2     10/06/97 7:33 Davec $ }

{ Written by David Churcher <dchurcher@cix.compulink.co.uk>,
  released to the public domain. }


{ A First-In First-Out circular buffer.
  Port of circbuf.c from Microsoft's Windows MIDI monitor example.
  I did do a version of this as an object (see Rev 1.1) but it was getting too 
  complicated and I couldn't see any real benefits to it so I dumped it 
  for an ordinary memory buffer with pointers. 

  This unit is a bit C-like, everything is done with pointers and extensive
  use is made of the undocumented feature of the Inc() function that 
  increments pointers by the size of the object pointed to.
  All of this could probably be done using Pascal array notation with
  range-checking turned off, but I'm not sure it's worth it.
}

Unit MSC_Circular_Buffer;

interface

Uses windows, MMSystem;

type
   { MIDI input event }
   TMidiBufferItem = record
      timestamp: uInt32;   { Timestamp in milliseconds after midiInStart }
      data: uInt32;        { MIDI message received }
      sysex: PMidiHdr;     { Pointer to sysex MIDIHDR, nil if not sysex }
   end;
   PMidiBufferItem = ^TMidiBufferItem;

   { MIDI input buffer }
   TCircularBuffer = record
      RecordHandle: HGLOBAL;     { Windows memory handle for this record }
      BufferHandle: HGLOBAL;     { Windows memory handle for the buffer }
      pStart: PMidiBufferItem;   { ptr to start of buffer }
      pEnd: PMidiBufferItem;     { ptr to end of buffer }
      pNextPut: PMidiBufferItem; { next location to fill }
      pNextGet: PMidiBufferItem; { next location to empty }
      Error: uInt16;             { error code from MMSYSTEM functions }
      Capacity: uInt16;          { buffer size (in TMidiBufferItems) }
      EventCount: uInt16;        { Number of events in buffer }
   end;

   PCircularBuffer = ^TCircularBuffer;

function GlobalSharedLockedAlloc( Capacity: uInt16; var hMem: HGLOBAL ): Pointer;
procedure GlobalSharedLockedFree( hMem: HGLOBAL; ptr: Pointer );

function CircbufAlloc( Capacity: uInt16 ): PCircularBuffer;
procedure CircbufFree( PBuffer: PCircularBuffer );
function CircbufRemoveEvent( PBuffer: PCircularBuffer ): Boolean;
function CircbufReadEvent( PBuffer: PCircularBuffer; PEvent: PMidiBufferItem ): Boolean;
{ Note: The PutEvent function is in the DLL }

implementation

{ Allocates in global shared memory, returns pointer and handle }
function GlobalSharedLockedAlloc (Capacity: uInt16; var hMem: HGLOBAL): Pointer;
var ptr: Pointer;
begin
   { Allocate the buffer memory }
   hMem := GlobalAlloc(GMEM_SHARE Or GMEM_MOVEABLE Or GMEM_ZEROINIT, Capacity );

   if (hMem = 0)
      then ptr := Nil
      else
   begin
      ptr := GlobalLock(hMem);
      if (ptr = Nil)
         then GlobalFree(hMem);
   end; // if

   GlobalSharedLockedAlloc := Ptr;
end; // GlobalSharedLockedAlloc //

procedure GlobalSharedLockedFree (hMem: HGLOBAL; ptr: Pointer);
begin
   if (hMem <> 0) then
   begin
      GlobalUnlock(hMem);
      GlobalFree(hMem);
   end; // if
end; // GlobalSharedLockedFree //

function CircbufAlloc (Capacity: uInt16): PCircularBuffer;
var NewCircularBuffer: PCircularBuffer;
    NewMIDIBuffer: PMidiBufferItem;
    hMem: HGLOBAL;
begin
   { TODO: Validate circbuf size, <64K }
   NewCircularBuffer :=
      GlobalSharedLockedAlloc (Sizeof(TCircularBuffer), hMem);
   if (NewCircularBuffer <> Nil) then
   begin
      NewCircularBuffer^.RecordHandle := hMem;
      NewMIDIBuffer :=
         GlobalSharedLockedAlloc( Capacity * Sizeof(TMidiBufferItem), hMem );
      if (NewMIDIBuffer = Nil) then
      begin
         // Exception here?
         GlobalSharedLockedFree( NewCircularBuffer^.RecordHandle,
                                 NewCircularBuffer );
         NewCircularBuffer := Nil;
      end else
      begin
                  NewCircularBuffer^.pStart := NewMidiBuffer;
         { Point to item at end of buffer }
         NewCircularBuffer^.pEnd := NewMidiBuffer;
         Inc(NewCircularBuffer^.pEnd, Capacity);
         { Start off the get and put pointers in the same position. These
           will get out of sync as the interrupts start rolling in }
         NewCircularBuffer^.pNextPut := NewMidiBuffer;
         NewCircularBuffer^.pNextGet := NewMidiBuffer;
         NewCircularBuffer^.Error := 0;
         NewCircularBuffer^.Capacity := Capacity;
         NewCircularBuffer^.EventCount := 0;
      end; // if
   end; // if
   CircbufAlloc := NewCircularBuffer;
end; // CircbufAlloc //

procedure CircbufFree (pBuffer: PCircularBuffer);
begin
   if (pBuffer <> Nil) then
   begin
      GlobalSharedLockedFree(pBuffer^.BufferHandle, pBuffer^.pStart);
      GlobalSharedLockedFree(pBuffer^.RecordHandle, pBuffer);
   end; // if
end; // CircbufFree //

{ Reads first event in queue without removing it.
  Returns true if successful, False if no events in queue }
function CircbufReadEvent (PBuffer: PCircularBuffer; PEvent: PMidiBufferItem): Boolean;
var PCurrentEvent: PMidiBufferItem;
begin
   if (PBuffer^.EventCount <= 0)
      then CircbufReadEvent := False
      else
   begin
      PCurrentEvent := PBuffer^.PNextget;

      { Copy the object from the "tail" of the buffer to the caller's object }
      PEvent^.Timestamp := PCurrentEvent^.Timestamp;
      PEvent^.Data := PCurrentEvent^.Data;
        PEvent^.Sysex := PCurrentEvent^.Sysex;
      CircbufReadEvent := True;
   end; // if
end; // CircbufReadEvent //

{ Remove current event from the queue }
function CircbufRemoveEvent (PBuffer: PCircularBuffer): Boolean;
begin
   if (PBuffer^.EventCount > 0) then
   begin
      Dec( Pbuffer^.EventCount);

      { Advance the buffer pointer, with wrap }
      Inc( Pbuffer^.PNextGet );
      If (PBuffer^.PNextGet = PBuffer^.PEnd) then
         PBuffer^.PNextGet := PBuffer^.PStart;

      CircbufRemoveEvent := True;
   end else CircbufRemoveEvent := False;
end; // CircbufRemoveEvent //

end. // Unit: MSC_Circular_Buffer //
