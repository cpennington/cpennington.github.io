---
title: Database Backed Configuration for Django
published: 2015-03-13
---

At [edX], we've gone through several evolutions of how we configure our primary django
application ([edx-platform][]). We started with a `settings.py`, as is standard for
django projects. I completed the migration from a single settings file to a directory
of environment-specific settings files in [this commit][1]. One of those environment
specific files was [aws.py][2] which read in specific settings from json configuration
files that we could deploy with configuration management software. That setup continues
to be the way we inject production configuration [to this day][3].

One downside of this style of configuration, however, is that it requires a redeploy/reboot
cycle to update the configuration. It would be better to have the configuration stored in
a central location that the system could read from (and update) on demand.

<!--more-->

The Requirements
----------------

After consultation with edX's operations folks, we agreed that ideally the system would:

1) Store the data in our central database.
2) Record the history of who changed values, and when (losing that information would
   be a big disadvantage when coming from a centralized version-control based configuration
   system).
3) Allow for rolling deployment.
4) Cache configured values, so that the database isn't constantly under load.
5) Be easy to extend to cover new types of configuration.
6) Have some sort of administration interface for managing the values.

None of the existing configuration-via-database solutions for django seemed to offer
all of those features, so I built a small django app to attempt to solve the problem.


Design Decisions
----------------

The obvious choice for solving requirements #1 and #5 was to use django Models to store
the configuration. This also meant that I could build on the django admin interface to
easily build #6. A sprinkle of django-cache satisfied #4.

By choosing to model configuration in django models, we had to then answer the question
of how to build new types of configuration. I opted for `ConfigurationModel` to be a
model base class, so that I could build common functionality around recording change
history into that class. I made the model [abstract][4] so that each configuration grouping
would have a separate table to track changes in.

That base abstract model provided a good place to ground common behavior to solve #2 (when paired
with some judicious features in the django admin views). It also gave a common location
to provide caching facilities.

The final constraint, rolling deployments, was handled as a side effect of using South
migrations. As long as the database is migrated before the servers, and as long as columns
are only added, then adding new configuration types is safe to deploy to live servers.


Using ConfigurationModels
-------------------------

One of edx-platforms current pieces of configuration is the set of users who have been
banned from the site (fortunately, a very short list). This is being stored in the
`UserStanding` model, reproduced below.

~~~ python
class UserStanding(models.Model):
    """
    This table contains a student's account's status.
    Currently, we're only disabling accounts; in the future we can imagine
    taking away more specific privileges, like forums access, or adding
    more specific karma levels or probationary stages.
    """
    ACCOUNT_DISABLED = "disabled"
    ACCOUNT_ENABLED = "enabled"
    USER_STANDING_CHOICES = (
        (ACCOUNT_DISABLED, u"Account Disabled"),
        (ACCOUNT_ENABLED, u"Account Enabled"),
    )

    user = models.ForeignKey(User, db_index=True, related_name='standing', unique=True)
    account_status = models.CharField(
        blank=True, max_length=31, choices=USER_STANDING_CHOICES
    )
    changed_by = models.ForeignKey(User, blank=True)
    standing_last_changed_at = models.DateTimeField(auto_now=True)
~~~

This table is checked before every page in the app, and, surprisingly, is responsible
for nearly 10% of our database time on production. We wanted to put it under a cache,
and it seemed convenient to pull it into our common configuration settings.

First, I created a `ConfigurationModel` subclass with a single field.

~~~ python
class UserStandingConfig(ConfigurationModel):
    disabled = models.TextField(
        help_text="White-space separated list of usernames of users who should be disabled",
        blank=True,
    )

    @property
    def disabled_list(self):
        return self.disabled.split()
~~~

The number of banned users is small, so the impact of parsing from the list frequently
should be minor. Future work will make even that work unnecessary.

Once the model is created, we can modify the middleware to use it.

~~~ diff
 class UserStandingMiddleware(object):
     def process_request(self, request):
         user = request.user
-        try:
-            user_account = UserStanding.objects.get(user=user.id)
-            # because user is a unique field in UserStanding, there will either be
-            # one or zero user_accounts associated with a UserStanding
-        except UserStanding.DoesNotExist:
-            pass
-        else:
-            if user_account.account_status == UserStanding.ACCOUNT_DISABLED:
-                msg = _(
-                    'Your account has been disabled. If you believe '
-                    'this was done in error, please contact us at '
-                    '{support_email}'
-                ).format(
-                    support_email=u'<a href="mailto:{address}?subject={subject_line}">{address}</a>'.format(
-                        address=settings.DEFAULT_FEEDBACK_EMAIL,
-                        subject_line=_('Disabled Account'),
-                    ),
-                )
-                return HttpResponseForbidden(msg)
+        config = UserStandingConfig.current()
+        if config.enabled and user.username in config.disabled_list:
+            msg = _(
+                'Your account has been disabled. If you believe '
+                'this was done in error, please contact us at '
+                '{support_email}'
+            ).format(
+                support_email=u'<a href="mailto:{address}?subject={subject_line}">{address}</a>'.format(
+                    address=settings.DEFAULT_FEEDBACK_EMAIL,
+                    subject_line=_('Disabled Account'),
+                ),
+            )
+            return HttpResponseForbidden(msg)
~~~

Key points to note: `UserStandingConfig.current()` returns the currently active configuration,
read from the cache. `.enabled` is a field provided by the `ConfigurationModel` base class, which
is used to provide a consistent way to enable/disable features on an individual basis.

Further changes (viewable in [the PR][5] against edx-platform) include deleting all of the existing
ui which allows for modification of the user standing, in favor of the built-in functionality
provided by the `ConfigurationModel` admin view. However, we need to wire that view to the model.

~~~ diff
# common/djangoapps/student/admin
 @@ -57,3 +58,5 @@ class Meta:
 admin.site.register(DashboardConfiguration, ConfigurationModelAdmin)

 admin.site.register(LinkedInAddToProfileConfiguration, LinkedInAddToProfileConfigurationAdmin)
+
+admin.site.register(UserStandingConfig, ConfigurationModelAdmin)
~~~

Future Work
-----------

A change that's in-progress is to allow for keyed `ConfigurationModels`. That is, allow for
configuration that varies depending on which (for example) user or course it applies to, and
is queried and cached separately for each, but with all of the same niceties provided for the
current `ConfigurationModel`. That would change the middleware code to just use

~~~ python
config = UserStandingConfig.current(user)
if config.account_disabled:
    ...
~~~

We would also like to convert more of edx-platform's existing configuration into `ConfigurationModels`,
to make it easier to get an instance up and running, and also to make it easier to manage the [edX][]
instances.


[edX]: http://edx.org
[edx-platform]: https://github.com/edx/edx-platform
[1]: https://github.com/edx/edx-platform/commit/a2eb8fcada01cce096cad8eabdeccb449149d4db
[2]: https://github.com/edx/edx-platform/blob/a2eb8fcada01cce096cad8eabdeccb449149d4db/envs/aws.py#L24-L25
[3]: https://github.com/edx/edx-platform/blob/master/lms/envs/aws.py#L116-L117
[4]: https://docs.djangoproject.com/en/1.4/topics/db/models/#abstract-base-classes
[5]: https://github.com/edx/edx-platform/pull/7283