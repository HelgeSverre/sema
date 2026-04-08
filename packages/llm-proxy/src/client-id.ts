function parseForwardedFor(value: string | null | undefined): string | null {
  if (!value) return null;
  const first = value.split(",")[0]?.trim();
  return first || null;
}

function normalizeClientId(value: string | null | undefined): string | null {
  const trimmed = value?.trim();
  return trimmed ? trimmed : null;
}

export function extractClientIdFromRequestHeaders(headers: Pick<Headers, "get">): string | null {
  return normalizeClientId(headers.get("cf-connecting-ip"))
    ?? parseForwardedFor(headers.get("x-forwarded-for"))
    ?? normalizeClientId(headers.get("x-real-ip"));
}

export function extractClientIdFromNodeHeaders(
  getHeader: (name: string) => string | null,
): string | null {
  return normalizeClientId(getHeader("cf-connecting-ip"))
    ?? parseForwardedFor(getHeader("x-forwarded-for"))
    ?? normalizeClientId(getHeader("x-real-ip"));
}
