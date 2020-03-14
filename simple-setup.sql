--
-- PostgreSQL database dump
--

-- Dumped from database version 10.5
-- Dumped by pg_dump version 10.5

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


--
-- Name: qualitative_length; Type: TYPE; Schema: public; Owner: jess
--

CREATE TYPE public.qualitative_length AS ENUM (
    'short',
    'medium',
    'long'
);


ALTER TYPE public.qualitative_length OWNER TO jess;

--
-- Name: trimmed_text; Type: DOMAIN; Schema: public; Owner: jess
--

CREATE DOMAIN public.trimmed_text AS text
	CONSTRAINT trimmed_text_check CHECK (((VALUE !~~ ' %'::text) AND (VALUE !~~ '% '::text)));


ALTER DOMAIN public.trimmed_text OWNER TO jess;

--
-- Name: foo_insert(text); Type: FUNCTION; Schema: public; Owner: jess
--

CREATE FUNCTION public.foo_insert(aa text) RETURNS text
    LANGUAGE plpgsql
    AS $$
declare
  result text;
begin
  insert into foo (a) values (substring(aa, '\S(?:.*\S)*')) returning a into result;
  return result;
end; $$;


ALTER FUNCTION public.foo_insert(aa text) OWNER TO jess;

--
-- Name: totalrecords(); Type: FUNCTION; Schema: public; Owner: jess
--

CREATE FUNCTION public.totalrecords() RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
total integer;
BEGIN
   SELECT count(*) into total FROM COMPANY;
   RETURN total;
END;
$$;


ALTER FUNCTION public.totalrecords() OWNER TO jess;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: collected_item; Type: TABLE; Schema: public; Owner: jess
--

CREATE TABLE public.collected_item (
    text text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.collected_item OWNER TO jess;

--
-- Name: jar_of_awesome; Type: TABLE; Schema: public; Owner: jess
--

CREATE TABLE public.jar_of_awesome (
    text text NOT NULL
);


ALTER TABLE public.jar_of_awesome OWNER TO jess;

--
-- Name: maybe; Type: TABLE; Schema: public; Owner: jess
--

CREATE TABLE public.maybe (
    text text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.maybe OWNER TO jess;

--
-- Name: next_action; Type: TABLE; Schema: public; Owner: jess
--

CREATE TABLE public.next_action (
    text text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    project public.trimmed_text
);


ALTER TABLE public.next_action OWNER TO jess;

--
-- Name: next_action_tag; Type: TABLE; Schema: public; Owner: jess
--

CREATE TABLE public.next_action_tag (
    next_action text NOT NULL,
    tag text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT next_action_tag_tag_check CHECK ((tag !~~ '%;%'::text))
);


ALTER TABLE public.next_action_tag OWNER TO jess;

--
-- Name: someday; Type: TABLE; Schema: public; Owner: jess
--

CREATE TABLE public.someday (
    text text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.someday OWNER TO jess;

--
-- Name: waiting_for; Type: TABLE; Schema: public; Owner: jess
--

CREATE TABLE public.waiting_for (
    text text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.waiting_for OWNER TO jess;

--
-- Name: collected_item collected_item_pkey; Type: CONSTRAINT; Schema: public; Owner: jess
--

ALTER TABLE ONLY public.collected_item
    ADD CONSTRAINT collected_item_pkey PRIMARY KEY (text);


--
-- Name: jar_of_awesome jar_of_awesome_pkey; Type: CONSTRAINT; Schema: public; Owner: jess
--

ALTER TABLE ONLY public.jar_of_awesome
    ADD CONSTRAINT jar_of_awesome_pkey PRIMARY KEY (text);


--
-- Name: maybe maybe_pkey; Type: CONSTRAINT; Schema: public; Owner: jess
--

ALTER TABLE ONLY public.maybe
    ADD CONSTRAINT maybe_pkey PRIMARY KEY (text);


--
-- Name: next_action next_action_pkey; Type: CONSTRAINT; Schema: public; Owner: jess
--

ALTER TABLE ONLY public.next_action
    ADD CONSTRAINT next_action_pkey PRIMARY KEY (text);


--
-- Name: someday someday_pkey; Type: CONSTRAINT; Schema: public; Owner: jess
--

ALTER TABLE ONLY public.someday
    ADD CONSTRAINT someday_pkey PRIMARY KEY (text);


--
-- Name: waiting_for waiting_for_pkey; Type: CONSTRAINT; Schema: public; Owner: jess
--

ALTER TABLE ONLY public.waiting_for
    ADD CONSTRAINT waiting_for_pkey PRIMARY KEY (text);


--
-- Name: next_action_tag next_action_tag_next_action_fkey; Type: FK CONSTRAINT; Schema: public; Owner: jess
--

ALTER TABLE ONLY public.next_action_tag
    ADD CONSTRAINT next_action_tag_next_action_fkey FOREIGN KEY (next_action) REFERENCES public.next_action(text);


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: postgres
--


--
-- PostgreSQL database dump complete
--

